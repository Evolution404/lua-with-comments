/*
** $Id: lparser.c,v 2.155 2016/08/01 19:51:24 roberto Exp $
** Lua Parser Lua分析器
** See Copyright Notice in lua.h
*/

// Lua的完整语法结构
// {A} 表示 0 或多个 A 
// [A] 表示一个可选的 A
// 大写字母表示一个终结符

// block 		-> statlist
// statlist 	-> { stat [';'] }
// stat 		-> LOCAL NAME {',' NAME} ['=' explist]  | RETURN [explist]
// 			| ';' (empty statement) | fistat | whilestat | DO block END | forstat | repeatstat 
// 			| funstat | localstat | label | retstat | breakstat | 'goto' NAME  | suffixedexp | assignment
// fieldsel 	-> ['.' | ':'] NAME
// index 		-> '[' expr ']'
// constructor -> '{' [ field { sep field } [sep] ] '}'
// sep 		-> ',' | ';'
// field 		-> listfield | recfield
// recfield 	-> (NAME | '['exp1']') = exp1
// listfield 	-> exp
// body 		->  '(' parlist ')' block END
// parlist 	-> [ param { ',' param } ]
// param 		-> NAME | '...'
// explist 	-> expr { ',' expr }
// funcargs 	-> '(' [ explist ] ')' | constructor | STRING
// primaryexp 	-> NAME | '(' expr ')'
// suffixedexp -> primaryexp { '.' fieldsel | '[' exp ']' | ':' NAME funcargs | funcargs }
// simpleexp 	-> FLT | INT | STRING | NIL | TRUE | FALSE | ... | constructor | FUNCTION body | suffixedexp
// subexpr 	-> (simpleexp | unop subexpr) { binop subexpr }
// assignment 	-> ',' suffixedexp assignment | '=' explist
// cond 		-> exp
// label 		-> '::' NAME '::'
// whilestat 	-> WHILE cond DO block END
// repeatstat 	-> REPEAT block UNTIL cond
// forstat 	-> FOR (fornum | forlist) END
// fornum 		-> NAME = exp1,exp1[,exp1] forbody 
// forlist 	-> NAME {,NAME} IN explist forbody
// forbody 	-> DO block
// ifstat 		-> IF cond THEN block { test_then_block } [ELSE block] END
// test_then_block -> [IF | ELSEIF] cond THEN block
// funcname 	-> NAME {fieldsel} [':' NAME]
// funcstat 	-> FUNCTION funcname body


#define lparser_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"



/* maximum number of local variables per function (must be smaller
   than 250, due to the bytecode format) */
// 每个函数的局部变量的最多的个数
#define MAXVARS		200


// 判断是否有多个返回值
// 只有函数调用和...会是多返回值
#define hasmultret(k)		((k) == VCALL || (k) == VVARARG)


/* because all strings are unified by the scanner, the parser
   can use pointer equality for string equality */
// 在luaX_newstring函数中确保了字符串一样地址就一样
// 该宏传入的TString对象一定由luaX_newstring创建而成,所以可以直接比较地址就能判断是否相等
#define eqstr(a,b)	((a) == (b))


/*
** nodes for block list (list of active blocks)
*/
// 多个BlockCnt类似FuncState也构成栈
// 块栈是基于当前函数而言,每个函数有独立的块栈,而FuncState栈每个文件唯一一个
typedef struct BlockCnt {
  // 当前块外部的块
  struct BlockCnt *previous;  /* chain */
  // 该块在Dyndata对象中第一个lable的下标
  int firstlabel;  /* index of first label in this block 块中第一个label */
  // 该块在Dyndata对象中第一个goto的下标
  int firstgoto;  /* index of first pending goto in this block 块中第一个goto */
  // 表示当前函数内在这个block之前的局部变量的个数
  lu_byte nactvar;  /* # active locals outside the block 该块外的局部变量 */
  // 表示这个block是否有upvalue被其它block访问
  lu_byte upval;  /* true if some variable in the block is an upvalue 标记是否有upvalue */
  // isloop只在leaveblock中起作用,用于生成一个名字为break的label
  lu_byte isloop;  /* true if 'block' is a loop 标记块是否是一个循环 */
} BlockCnt;



/*
** prototypes for recursive non-terminal functions
*/
// 递归非终结符的函数原型
static void statement (LexState *ls);
static void expr (LexState *ls, expdesc *v);


/* semantic error 语法错误 */
static l_noret semerror (LexState *ls, const char *msg) {
  ls->t.token = 0;  /* remove "near <token>" from final message */
  luaX_syntaxerror(ls, msg);
}


static l_noret error_expected (LexState *ls, int token) {
  luaX_syntaxerror(ls,
      luaO_pushfstring(ls->L, "%s expected", luaX_token2str(ls, token)));
}


static l_noret errorlimit (FuncState *fs, int limit, const char *what) {
  lua_State *L = fs->ls->L;
  const char *msg;
  int line = fs->f->linedefined;
  const char *where = (line == 0)
                      ? "main function"
                      : luaO_pushfstring(L, "function at line %d", line);
  msg = luaO_pushfstring(L, "too many %s (limit is %d) in %s",
                             what, limit, where);
  luaX_syntaxerror(fs->ls, msg);
}


// v是当前值 l是limit(上限)
// 检查v是否超过了l 超过了就报错
static void checklimit (FuncState *fs, int v, int l, const char *what) {
  if (v > l) errorlimit(fs, l, what);
}

// test是返回传入符号与当前符号比较结果,check是如果传入符号不符合直接报错
// 函数名带next说明如果符号相同读取下一个符号

// 检查当前符号是不是c,如果是,跳过当前符号读取下一个符号
// 返回1 当前符号是c 读取下一个符号
// 返回0 当前符号不是c 没有其他操作
static int testnext (LexState *ls, int c) {
  if (ls->t.token == c) {
    luaX_next(ls);
    return 1;
  }
  else return 0;
}


// 保证当前符号是c 不是c将报错
static void check (LexState *ls, int c) {
  if (ls->t.token != c)
    error_expected(ls, c);
}


// 当前符号是c 读取下一个符号
// 当前符号不是c 报错
static void checknext (LexState *ls, int c) {
  check(ls, c);
  luaX_next(ls);
}


// c为真报错 c为假无操作
#define check_condition(ls,c,msg)	{ if (!(c)) luaX_syntaxerror(ls, msg); }



// 当前符号是what 读取下一个符号
// 当前符号不是what 报错
static void check_match (LexState *ls, int what, int who, int where) {
  if (!testnext(ls, what)) {
    if (where == ls->linenumber)
      error_expected(ls, what);
    else {
      luaX_syntaxerror(ls, luaO_pushfstring(ls->L,
             "%s expected (to close %s at line %d)",
              luaX_token2str(ls, what), luaX_token2str(ls, who), where));
    }
  }
}


// 当前符号是TK_NAME 返回当前符号TString对象 并读取下一个符号
// 当前符号不是TK_NAME 报错
static TString *str_checkname (LexState *ls) {
  TString *ts;
  check(ls, TK_NAME);
  ts = ls->t.seminfo.ts;
  luaX_next(ls);
  return ts;
}


// 初始化一个表达式对象 指定类型和info
static void init_exp (expdesc *e, expkind k, int i) {
  e->f = e->t = NO_JUMP;
  e->k = k;
  e->u.info = i;
}


// 将字符串s加入常量表 设置表达式e为常量
static void codestring (LexState *ls, expdesc *e, TString *s) {
  init_exp(e, VK, luaK_stringK(ls->fs, s));
}


// 确保当前符号是TK_NAME
// 设置表达式为VK类型,info指向变量名在常量表的位置
static void checkname (LexState *ls, expdesc *e) {
  codestring(ls, e, str_checkname(ls));
}


// 将varname加入到ls->fs->f->locvars数组中
// 返回新增局部变量的下标
static int registerlocalvar (LexState *ls, TString *varname) {
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int oldsize = f->sizelocvars;
  // 增长f->locvars的空间 Proto对象中局部变量的个数在FuncState中即fs->nlocvars
  luaM_growvector(ls->L, f->locvars, fs->nlocvars, f->sizelocvars,
                  LocVar, SHRT_MAX, "local variables");
  while (oldsize < f->sizelocvars)  // 初始化增长的空间
    f->locvars[oldsize++].varname = NULL;
  f->locvars[fs->nlocvars].varname = varname;  // 将传入的变量名放到最后一个位置
  luaC_objbarrier(ls->L, f, varname);
  return fs->nlocvars++;
}


// 创建一个名称为name的局部变量
// 新建一个变量需要修改Dyndata(dyd),FuncState(fs),Proto(f)三个对象
// 在fs->f->locvars数组中新增一项,保存变量名
// 让fs->nlocvars自增1
// 在dyd->actvar.arr中新增一项,保存新增变量在fs->f->locvars中的下标
static void new_localvar (LexState *ls, TString *name) {
  FuncState *fs = ls->fs;
  Dyndata *dyd = ls->dyd;
  int reg = registerlocalvar(ls, name);  // reg是name变量在f->locvars中的下标
  // 当前函数的局部变量的个数没有过多
  // dyd->actvar.n-fs->firstlocal是当前局部变量的个数 加一确保增加一个变量也不越界
  checklimit(fs, dyd->actvar.n + 1 - fs->firstlocal,
                  MAXVARS, "local variables");
  luaM_growvector(ls->L, dyd->actvar.arr, dyd->actvar.n + 1,
                  dyd->actvar.size, Vardesc, MAX_INT, "local variables");
  // fs->f->locvars[idx]可以访问到局部变量
  dyd->actvar.arr[dyd->actvar.n++].idx = cast(short, reg);  // 保存局部变量的下标idx
}


static void new_localvarliteral_ (LexState *ls, const char *name, size_t sz) {
  new_localvar(ls, luaX_newstring(ls, name, sz));
}

// 通过字符串字面量创建一个局部变量
#define new_localvarliteral(ls,v) \
	new_localvarliteral_(ls, "" v, (sizeof(v)/sizeof(char))-1)


// 获取第i个局部变量 i从0开始
static LocVar *getlocvar (FuncState *fs, int i) {
  // 得到fs->f中的下标
  int idx = fs->ls->dyd->actvar.arr[fs->firstlocal + i].idx;
  lua_assert(idx < fs->nlocvars);
  return &fs->f->locvars[idx];  // 获取局部变量
}


// fs->nactvar+=nvars
// 为新增加的局部变量设置startpc为当前fs->pc
static void adjustlocalvars (LexState *ls, int nvars) {
  FuncState *fs = ls->fs;
  fs->nactvar = cast_byte(fs->nactvar + nvars);
  for (; nvars; nvars--) {  // 设置新增加的局部变量的startpc
    getlocvar(fs, fs->nactvar - nvars)->startpc = fs->pc;
  }
}


// 设置活动的局部变量的个数为tolevel个
// 并设置删除变量的endpc为当前fs->pc
static void removevars (FuncState *fs, int tolevel) {
  fs->ls->dyd->actvar.n -= (fs->nactvar - tolevel);  // 设置活跃局部变量的个数
  while (fs->nactvar > tolevel)  // 设置endpc
    getlocvar(fs, --fs->nactvar)->endpc = fs->pc;
}


// 查询upvalue在fs->f->upvalues中的下标并返回
// 返回-1代表查询失败未找到
static int searchupvalue (FuncState *fs, TString *name) {
  int i;
  Upvaldesc *up = fs->f->upvalues;
  for (i = 0; i < fs->nups; i++) {
    if (eqstr(up[i].name, name)) return i;
  }
  return -1;  /* not found */
}


// 根据表达式信息和名称创建一个upvalue
// 返回新增的upvalue的下标
// upvalue存放在f->upvalues中
static int newupvalue (FuncState *fs, TString *name, expdesc *v) {
  Proto *f = fs->f;
  int oldsize = f->sizeupvalues;
  checklimit(fs, fs->nups + 1, MAXUPVAL, "upvalues");  // 空间确保没到上限
  // 如果有必要则申请新的空间
  luaM_growvector(fs->ls->L, f->upvalues, fs->nups, f->sizeupvalues,
                  Upvaldesc, MAXUPVAL, "upvalues");
  while (oldsize < f->sizeupvalues)  // 初始化新空间
    f->upvalues[oldsize++].name = NULL;
  f->upvalues[fs->nups].instack = (v->k == VLOCAL);  // 输入表达式是一个局部变量就标记instack是1
  f->upvalues[fs->nups].idx = cast_byte(v->u.info);  // 保存位置信息
  f->upvalues[fs->nups].name = name;                 // 保存变量名字
  luaC_objbarrier(fs->ls->L, f, name);
  return fs->nups++;
}


// 查询局部变量n的下标并返回 -1说明没有找到
static int searchvar (FuncState *fs, TString *n) {
  int i;
  for (i = cast_int(fs->nactvar) - 1; i >= 0; i--) {
    if (eqstr(n, getlocvar(fs, i)->varname))
      return i;
  }
  return -1;  /* not found */
}


/*
  Mark block where variable at given level was defined
  (to emit close instructions later).
*/
// level是在fs->f->locvars中的下标,也就是局部变量
// 这个函数是用来标记定义level变量的块的upval字段为1,说明该块有定义的变量成为upvalue
// 也就是说level变量在它被定义的块内部某处成为了upvalue
static void markupval (FuncState *fs, int level) {
  BlockCnt *bl = fs->bl;
  // 从当前解析所在的块向外查询直到找到定义level变量的块
  // nactvar含义是当前块外部定义的局部变量的个数,越往外nactvar越小
  // level=3代表是这个函数定义的第4个局部变量,如果bl->nactvar=3代表该块前面有3个局部变量
  // 也就是说level=3这个变量就在该块内定义
  while (bl->nactvar > level)
    bl = bl->previous;
  // 找到了定义level的块,标记upval为1
  bl->upval = 1;
}


/*
  Find variable with given name 'n'. If it is an upvalue, add this
  upvalue into all intermediate functions.
*/
// 查询变量n 将变量类型和位置写入var中,base=1表示直接调用,base=0表示是在递归调用
// 先查询局部变量再递归查询upvalue
// 执行后var->k == VVOID说明是全局变量
static void singlevaraux (FuncState *fs, TString *n, expdesc *var, int base) {
  if (fs == NULL)  /* no more levels? 递归查询的终点 */
    init_exp(var, VVOID, 0);  /* default is global 标记为VVOID到全局_ENV变量中查询 */
  else {
    // 在当前函数的局部变量中查询变量n
    int v = searchvar(fs, n);  /* look up locals at current level */
    if (v >= 0) {  /* found? 查询到变量n */
      init_exp(var, VLOCAL, v);  /* variable is local 是一个局部变量 */
      if (!base)  // base==0代表现在是递归查询 这个变量被内部某处引用了需要标记这个变量所在块 upval=1
        markupval(fs, v);  /* local will be used as an upval */
    }
    else {  /* not found as local at current level; try upvalues 没有查询到变量n 查询upvalue */
      int idx = searchupvalue(fs, n);  /* try existing upvalues */
      if (idx < 0) {  /* not found? */
        singlevaraux(fs->prev, n, var, 0);  /* try upper levels 到上一层函数中查询变量 */
        // 递归查询之后还是没有找到 说明是全局变量
        if (var->k == VVOID)  /* not found? */
          return;  /* it is a global 直接返回等singlevar函数处理 */
        /* else was LOCAL or UPVAL 递归查询找到了 */
        // 创建一个新的upvalue
        idx  = newupvalue(fs, n, var);  /* will be a new upvalue */
      }
      // 初始化表达式 可能是新建的upvalue也可能原来就有
      init_exp(var, VUPVAL, idx);  /* new or old upvalue */
    }
  }
}


// 当前符号是TK_NAME,该函数用来查询这个变量名后初始化表达式var,并读入下一个符号
// 变量名有三种情况,局部变量,upvalue和没有查询到
// 局部变量 var->k=VLOCAL,var->u.info=寄存器位置
// upvalue  var->k=VUPVAL,var->u.info=upvalue位置
// 没有查询到就到_ENV表(全局变量)中查询 var->k=VINDEXED,设置var->u.ind
static void singlevar (LexState *ls, expdesc *var) {
  // 读取一个变量名
  TString *varname = str_checkname(ls);
  FuncState *fs = ls->fs;
  singlevaraux(fs, varname, var, 1);
  // VVOID说明没有找到 到全局变量中找
  if (var->k == VVOID) {  /* global name? 全局变量 */
    expdesc key;
    singlevaraux(fs, ls->envn, var, 1);  /* get environment variable 获取_ENV变量 */
    lua_assert(var->k != VVOID);  /* this one must exist */
    // 初始化key为VK类型 将varname放入常量表 key->u.info存放varname在常量表的位置
    codestring(ls, &key, varname);  /* key is variable name */
    // 设置var为VINDEXED
    luaK_indexed(fs, var, &key);  /* env[varname] */
  }
}


// 调整赋值语句等号前后变量和表达式个数的问题 
// nvar变量的个数 nexps表达式的个数
// 变量多于表达式 多的变量赋值为nil
// 表达式多于变量 多的表达式直接丢弃
static void adjust_assign (LexState *ls, int nvars, int nexps, expdesc *e) {
  FuncState *fs = ls->fs;
  int extra = nvars - nexps;
  // 表达式类型是VCALL或VVARARG
  // 调用函数或者使用了...变量进行赋值,那么结果个数是变化的
  if (hasmultret(e->k)) {
    // extra+1代表这个多结果表达式的个数
    // 例如 local a,b,c = 1,f(),那么f()应该返回两个值,extra为1所以extra+1代表f()的返回个数
    extra++;  /* includes call itself */
    if (extra < 0) extra = 0;
    // 指定表达式的返回值个数为extra
    luaK_setreturns(fs, e, extra);  /* last exp. provides the difference */
    // 对于VCALL或VVARARG类型在执行前已经分配了nexps个寄存器
    // 现在还需要nvars-nexps个,也就是extra-1个
    // 正常情况表达式不是多返回类型,是分配了nexps-1个
    // 但是VCALL类型被调用的函数也要占用一个寄存器,在suffixedexp中存入寄存器
    // VVARARG类型在上一行luaK_setmultret额外分配了一个寄存器
    // 所以最终减少了一个寄存器
    if (extra > 1) luaK_reserveregs(fs, extra-1);
  }
  else {
    // 处理掉表达式e 将表达式的值存入寄存器
    if (e->k != VVOID) luaK_exp2nextreg(fs, e);  /* close last expression */
    if (extra > 0) {  // 如果变量比表达式多 多的变量赋值为nil
      int reg = fs->freereg;
      luaK_reserveregs(fs, extra);
      luaK_nil(fs, reg, extra);  // 从reg开始extra个寄存器设置为nil
    }
  }
  if (nexps > nvars)  // 表达式比变量多 直接放弃识别的多余的表达式
    ls->fs->freereg -= nexps - nvars;  /* remove extra values */
}


// enterlevel与leavelevel成对出现,用来记录现在c语言调用栈的深度,超过限制后报错
// L->nCcalls++ 检查nCcalls是否超过最大值
static void enterlevel (LexState *ls) {
  lua_State *L = ls->L;
  ++L->nCcalls;
  checklimit(ls->fs, L->nCcalls, LUAI_MAXCCALLS, "C levels");
}


#define leavelevel(ls)	((ls)->L->nCcalls--)

// 以下是goto语句和label语句的识别,这两者成对存在
// label语句使用labelstat识别
// goto语句使用gotostat识别
// ls->dyd->gt存放goto列表
// ls->dyd->label存放label列表

// g是goto语句在gotolist的下标
// label是该goto语句对应的label
// 该函数就是用来回填goto语句对应的JMP指令,并在gotolist中删除该goto语句
static void closegoto (LexState *ls, int g, Labeldesc *label) {
  int i;
  FuncState *fs = ls->fs;
  Labellist *gl = &ls->dyd->gt;  // gotolist 存放goto的数组
  Labeldesc *gt = &gl->arr[g];   // 当前要关闭的实际的goto
  lua_assert(eqstr(gt->name, label->name));  // 确保传入的label和goto是匹配的
  // goto位置的变量个数不能小于label位置的变量个数
  // 也就是goto不能向后跳且中间有变量定义
  if (gt->nactvar < label->nactvar) {
    TString *vname = getlocvar(fs, gt->nactvar)->varname;
    const char *msg = luaO_pushfstring(ls->L,
      "<goto %s> at line %d jumps into the scope of local '%s'",
      getstr(gt->name), gt->line, getstr(vname));
    semerror(ls, msg);
  }
  // 跳转目标是label语句所在的指令
  luaK_patchlist(fs, gt->pc, label->pc);
  /* remove goto from pending list */
  // 在gotolist中删除当前goto语句
  for (i = g; i < gl->n - 1; i++)
    gl->arr[i] = gl->arr[i + 1];
  gl->n--;
}


/*
** try to close a goto with existing labels; this solves backward jumps
*/
// 尝试寻找与goto语句匹配的label语句
// 寻找到回填JMP指令,否则不做操作
static int findlabel (LexState *ls, int g) {
  int i;
  BlockCnt *bl = ls->fs->bl;
  Dyndata *dyd = ls->dyd;
  Labeldesc *gt = &dyd->gt.arr[g];  // 找到要查询的goto
  /* check labels in current block for a match */
  // 在当前块中查询goto匹配的lable
  for (i = bl->firstlabel; i < dyd->label.n; i++) {  // 遍历当前块内的label
    Labeldesc *lb = &dyd->label.arr[i];  // 拿到label的信息
    if (eqstr(lb->name, gt->name)) {  /* correct label? lable和goto的名称相等 */
      // goto在label后面,除非label在外部块且当前块没有upvalue否则要关闭goto与label之间的变量
      if (gt->nactvar > lb->nactvar &&
          (bl->upval || dyd->label.n > bl->firstlabel))
        luaK_patchclose(ls->fs, gt->pc, lb->nactvar);
      closegoto(ls, g, lb);  /* close it */
      return 1;
    }
  }
  return 0;  /* label not found; cannot close goto */
}


// 在Labellist对象l中新增加一个label项
// 返回新增项的下标
static int newlabelentry (LexState *ls, Labellist *l, TString *name,
                          int line, int pc) {
  int n = l->n;
  // 如果满了就增长空间
  luaM_growvector(ls->L, l->arr, n, l->size,
                  Labeldesc, SHRT_MAX, "labels/gotos");
  // 在数组中新增一个label
  l->arr[n].name = name;
  l->arr[n].line = line;
  l->arr[n].nactvar = ls->fs->nactvar;
  l->arr[n].pc = pc;
  l->n = n + 1;
  return n;
}


/*
** check whether new label 'lb' matches any pending gotos in current
** block; solves forward jumps
*/
// 传入label,在当前块待回填goto中寻找对应的项进行回填
static void findgotos (LexState *ls, Labeldesc *lb) {
  // 获得gotolist
  Labellist *gl = &ls->dyd->gt;
  // 获得当前块的第一个goto语句
  int i = ls->fs->bl->firstgoto;
  // 遍历当前块内的待回填goto语句
  // 可能回填多个goto语句
  while (i < gl->n) {
    if (eqstr(gl->arr[i].name, lb->name))
      closegoto(ls, i, lb);
    else
      i++;
  }
}


/*
** export pending gotos to outer level, to check them against
** outer labels; if the block being exited has upvalues, and
** the goto exits the scope of any variable (which can be the
** upvalue), close those variables being exited.
*/
// 该函数只在leaveblock中调用
// 调用时bl是当前块,fs->bl是当前块的外部块
// 在离开块是将待回填的goto语句移动到当前块的外部块内
static void movegotosout (FuncState *fs, BlockCnt *bl) {
  int i = bl->firstgoto;
  Labellist *gl = &fs->ls->dyd->gt;
  /* correct pending gotos to current block and try to close it
     with visible labels */
  // 遍历当前块内的goto
  while (i < gl->n) {
    Labeldesc *gt = &gl->arr[i];
    // goto语句前面定义了变量, 而且这个当前块有被引用的upvalue
    // 那么让goto语句对应的跳转指令关闭当前块内的变量
    // 因为goto语句在当前块没有匹配的label,就代表着一定要跳转到块外部
    // 跳转到块外部那么块内的这些变量失效,要关闭这些upvalue
    if (gt->nactvar > bl->nactvar) {
      if (bl->upval)
        // 写入bl->nactvar代表关闭当前块内的变量
        luaK_patchclose(fs, gt->pc, bl->nactvar);
      gt->nactvar = bl->nactvar;
    }
    // 在leaveblock函数中调用movegotosout之前已经让fs->bl=bl->previous
    // 所以现在调用findlabel是在当前块的外部块中查询匹配的label
    // 如果这里不查询外部块是否有匹配的label的话,会造成goto有可能不匹配,例如
    // ::a::
    // if true then
    //   goto a
    // end
    // 在离开if块时如果不查询goto a匹配的label,由于分析器是一遍过就不会有机会再匹配了
    // 搜索到匹配的label,i不变,因为findlabel函数内修改了gl->arr数组,所以下一次循环是下一个goto
    // 搜索不到匹配的label那么就让i自增,循环下一个goto
    if (!findlabel(fs->ls, i))
      i++;  /* move to next one */
  }
}


// 初始化一个块bl,在fs->bl链表上新增当前输入的bl
// isloop标记该块是否是循环
static void enterblock (FuncState *fs, BlockCnt *bl, lu_byte isloop) {
  bl->isloop = isloop;
  bl->nactvar = fs->nactvar;
  bl->firstlabel = fs->ls->dyd->label.n;
  bl->firstgoto = fs->ls->dyd->gt.n;
  bl->upval = 0;
  bl->previous = fs->bl;
  fs->bl = bl;
  lua_assert(fs->freereg == fs->nactvar);
}


/*
** create a label named 'break' to resolve break statements
*/
// 用pc回填跳转目标为break的goto语句
static void breaklabel (LexState *ls) {
  TString *n = luaS_new(ls->L, "break");
  // 先生成一个名为break的label语句
  // 生成break的label语句不需要调用checkrepeated
  // 因为每个块只会在调用leaveblock时才会生成一个break的label语句,放入ls->dyd->label中
  // 这里生成的break的label在该块退出后(调用时leaveblock后),就会从ls->dyd->label中移除
  int l = newlabelentry(ls, &ls->dyd->label, n, 0, ls->fs->pc);
  // 查询与其对应的goto语句
  findgotos(ls, &ls->dyd->label.arr[l]);
}

/*
** generates an error for an undefined 'goto'; choose appropriate
** message when label name is a reserved word (which can only be 'break')
*/
static l_noret undefgoto (LexState *ls, Labeldesc *gt) {
  const char *msg = isreserved(gt->name)
                    ? "<%s> at line %d not inside a loop"
                    : "no visible label '%s' for <goto> at line %d";
  msg = luaO_pushfstring(ls->L, msg, getstr(gt->name), gt->line);
  semerror(ls, msg);
}


static void leaveblock (FuncState *fs) {
  BlockCnt *bl = fs->bl;
  LexState *ls = fs->ls;
  // 这个块有声明的变量成为了它内部某块的upvalue,现在要离开这个块也就是这个变量脱离作用域了
  // 这个upvalue需要被关闭,保存到UpVal结构里面
  // 如果bl->previous是NULL,很显然现在处理的块是最外部的块,这里就没有必要再关闭了
  // local a = {} -- an empty array
  // local x = 10
  // for i = 1, 2 do
  //   local j = i
  //   a[i] = function () return x + j end
  // end
  // x = 20
  // 如上示例代码,循环块中变量j在离开块时就需要被关闭,a[1]和a[2]中使用了不同的j
  // 最后修改了x=20,a[1]和a[2]内部的x会同时修改
  if (bl->previous && bl->upval) {
    /* create a 'jump to here' to close upvalues */
    int j = luaK_jump(fs);
    // 用bl->nactvar+1回填j链的参数A
    luaK_patchclose(fs, j, bl->nactvar);
    luaK_patchtohere(fs, j);
  }
  // break语句只有在循环块中才有效
  // 如果是循环块查询是否有break语句,生成跳转指令
  if (bl->isloop)
    breaklabel(ls);  /* close pending breaks */
  // 在fs->bl链上删除最后一个
  fs->bl = bl->previous;
  // 离开当前块后,外面块的局部变量个数与定义当前块的时候一样
  // 所以设置局部变量的个数为bl->nactvar
  removevars(fs, bl->nactvar);
  lua_assert(bl->nactvar == fs->nactvar);
  // 释放掉这个块用到的临时寄存器
  fs->freereg = fs->nactvar;  /* free registers */
  // 离开当前块,当前块内定义的label失效
  ls->dyd->label.n = bl->firstlabel;  /* remove local labels */
  // 离开当前块,如果还有外部块直接将goto拷贝到外部块
  if (bl->previous)  /* inner block? */
    movegotosout(fs, bl);  /* update pending gotos to outer block */
  // bl->firstgoto在enterblock设置为ls->dyd->gt.n
  // bl->firstgoto<ls->dyd->gt.n代表该块定义了其他的goto
  // 没有外部块,而且当前块内还有剩余的goto
  // 那么就代表这个goto语句没有匹配的label语句
  else if (bl->firstgoto < ls->dyd->gt.n)  /* pending gotos in outer block? */
    // 外部没有块,还有待匹配的goto说明这个goto不能被匹配,报错
    undefgoto(ls, &ls->dyd->gt.arr[bl->firstgoto]);  /* error */
}


/*
** adds a new prototype into list of prototypes
*/
// 往当前处理函数中增加一个子函数(即将进行分析的函数)
// 往ls->fs->f->p里增加一个Proto指针,ls->fs->f->np++ 返回增加的Proto指针
// Proto->p是一个二级指针 这里其实是一个Proto对象地址的数组
// 真实的空间是在luaF_newproto申请的
static Proto *addprototype (LexState *ls) {
  // 新增的子函数
  Proto *clp;
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  // 当前的函数
  Proto *f = fs->f;  /* prototype of current function */
  if (fs->np >= f->sizep) {  // 数组已经放满了
    // 增加Proto数组的空间
    int oldsize = f->sizep;
    luaM_growvector(L, f->p, fs->np, f->sizep, Proto *, MAXARG_Bx, "functions");
    while (oldsize < f->sizep)
      f->p[oldsize++] = NULL;
  }
  // 往子函数(p)数组中增加新建的Proto对象
  f->p[fs->np++] = clp = luaF_newproto(L);
  luaC_objbarrier(L, f, clp);
  return clp;
}


/*
** codes instruction to create new closure in parent function.
** The OP_CLOSURE instruction must use the last available register,
** so that, if it invokes the GC, the GC knows which registers
** are in use at that time.
*/
// fs为当前识别函数的外部函数,生成OP_CLOSURE指令
// 参数A为fs->freereg,参数Bx为fs->np-1也就是当前函数在子函数表的下标
// 设置表达是v为VNONRELOC类型
static void codeclosure (LexState *ls, expdesc *v) {
  // 找到外部的函数
  FuncState *fs = ls->fs->prev;
  // fs->np-1就是当前函数在其外部函数存放内部函数表的下标
  // 将表达式v设置成VRELOCABLE类型 OP_CLOSURE指令需要回填参数A
  init_exp(v, VRELOCABLE, luaK_codeABx(fs, OP_CLOSURE, 0, fs->np - 1));
  // 回填OP_CLOSURE指令,参数A是freereg,然后freereg++
  luaK_exp2nextreg(fs, v);  /* fix it at the last register */
}


// 输入fs,bl是刚刚新建的对象,该函数用来初始化fs和bl
// 调用之前应该分别新建FuncState,BlockCnt以及Proto对象
// 将当前fs与其外部函数(ls->fs)串起来 让ls->fs=fs
static void open_func (LexState *ls, FuncState *fs, BlockCnt *bl) {
  Proto *f;
  // 以下三句用来处理fs,fs->prev,ls之间的关系
  // ls->fs代表外部的FuncState
  // fs->prev指向外部FuncState
  fs->prev = ls->fs;  /* linked list of funcstates */
  // 修改当前FuncState的LexState
  fs->ls = ls;
  // 修改LexState的FuncState为当前FuncState
  ls->fs = fs;
  fs->pc = 0;
  fs->lasttarget = 0;
  fs->jpc = NO_JUMP;
  fs->freereg = 0;
  fs->nk = 0;
  fs->np = 0;
  fs->nups = 0;
  fs->nlocvars = 0;
  fs->nactvar = 0;
  fs->firstlocal = ls->dyd->actvar.n;
  fs->bl = NULL;
  // 处理Proto对象
  f = fs->f;
  f->source = ls->source;
  // 一个函数最少占用两个栈空间
  f->maxstacksize = 2;  /* registers 0/1 are always valid */
  // 初始化bl
  enterblock(fs, bl, 0);
}


// 与open_func对应,退出当前函数,生成OP_RETURN指令
// 重新分配各个数组让它们正好放满,避免浪费空间
static void close_func (LexState *ls) {
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  // 每个函数最后一条指令都是RETURN 0 1
  // OP_RETURN指令的参数
  luaK_ret(fs, 0, 0);  /* final return */
  leaveblock(fs);
  // 避免浪费空间 将各个数组的大小都重新分配成它当前存的元素个数
  // 重新分配指令数组
  luaM_reallocvector(L, f->code, f->sizecode, fs->pc, Instruction);
  f->sizecode = fs->pc;
  // 重新分配行信息数组
  luaM_reallocvector(L, f->lineinfo, f->sizelineinfo, fs->pc, int);
  f->sizelineinfo = fs->pc;
  // 重新分配常量数组
  luaM_reallocvector(L, f->k, f->sizek, fs->nk, TValue);
  f->sizek = fs->nk;
  // 重新分配子函数数组
  luaM_reallocvector(L, f->p, f->sizep, fs->np, Proto *);
  f->sizep = fs->np;
  // 重新分配局部变量数组
  luaM_reallocvector(L, f->locvars, f->sizelocvars, fs->nlocvars, LocVar);
  f->sizelocvars = fs->nlocvars;
  // 重新分配upvalue数组
  luaM_reallocvector(L, f->upvalues, f->sizeupvalues, fs->nups, Upvaldesc);
  f->sizeupvalues = fs->nups;
  // 执行该函数时应该只有块栈应该只有一层,调用过leaveblock后fs->bl应为NULL
  lua_assert(fs->bl == NULL);
  // 退出当前函数
  ls->fs = fs->prev;
  luaC_checkGC(L);
}



/*============================================================*/
/* GRAMMAR RULES */
/*============================================================*/


/*
** check whether current token is in the follow set of a block.
** 'until' closes syntactical blocks, but do not close scope,
** so it is handled in separate.
*/
// 检查当前符号是块结束的符号
// 也就是检查当前符号是否是TK_ELSE,TK_ELSEIF,TK_END,TK_EOS,TK_UNTIL
// 如果是则返回1 否则返回0
// 由于until表示句法上的结束,吊饰没有关闭变量的作用域所以单独处理,withuntil标记是否结束块
static int block_follow (LexState *ls, int withuntil) {
  switch (ls->t.token) {
    case TK_ELSE: case TK_ELSEIF:
    case TK_END: case TK_EOS:
      return 1;
    case TK_UNTIL: return withuntil;
    default: return 0;
  }
}


// 识别多条语句 可以发现lua支持语句使用;(分号)分割
static void statlist (LexState *ls) {
  /* statlist -> { stat [';'] } */
  // 读取到TK_ELSE,TK_ELSEIF,TK_END,TK_EOS,TK_UNTIL停止循环
  // 在每次调用statement的时候都有fs->nactvar==fs->freereg
  while (!block_follow(ls, 1)) {
    // 识别到return符号就继续识别这条语句 但是识别完成直接返回下面没必要识别了
    if (ls->t.token == TK_RETURN) {
      statement(ls);
      // 识别完return语句直接返回 下面的语句直接丢掉
      return;  /* 'return' must be last statement */
    }
    statement(ls);
  }
}


// 识别.或: 再识别一个变量名,v保存了点或冒号前的表达式
// 修改表达式v为v[NAME] NAME就是新识别出来的变量
static void fieldsel (LexState *ls, expdesc *v) {
  /* fieldsel -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  expdesc key;
  luaK_exp2anyregup(fs, v);  // 把原来的表达式存到寄存器
  luaX_next(ls);  /* skip the dot or colon 跳过点或冒号 */
  checkname(ls, &key);  // 读取点或冒号后的变量名,设置key为VK类型
  // 设置v为v[key]
  // 设置v的类型为VINDEXED
  luaK_indexed(fs, v, &key);
}


// 识别 [ expr ]
// 输入时当前符号是[, 执行完毕当前符号指向]下一个
static void yindex (LexState *ls, expdesc *v) {
  /* index -> '[' expr ']' */
  luaX_next(ls);  /* skip the '[' */
  expr(ls, v);
  luaK_exp2val(ls->fs, v);
  checknext(ls, ']');
}


/*
** {======================================================================
** Rules for Constructors
** =======================================================================
*/


// 识别表结构过程中使用的结构体
struct ConsControl {
  expdesc v;  /* last list item read */
  expdesc *t;  /* table descriptor */
  // 哈希部分的大小
  int nh;  /* total number of 'record' elements */
  // 数组部分的大小
  int na;  /* total number of array elements */
  int tostore;  /* number of array elements pending to be stored */
};


// recfield是{a=1,b=2,c=3}这种类型带=
// 该函数识别一个=的部分
// 开始执行时当前符号可能是[或者TK_NAME,结束时当前符号指向,或}
// 生成OP_SETTABLE指令 cc->nh++
static void recfield (LexState *ls, struct ConsControl *cc) {
  /* recfield -> (NAME | '['exp1']') = exp1 */
  FuncState *fs = ls->fs;
  int reg = ls->fs->freereg;
  expdesc key, val;
  int rkkey;
  if (ls->t.token == TK_NAME) {  // 输入时是TK_NAME
    checklimit(fs, cc->nh, MAX_INT, "items in a constructor");
    checkname(ls, &key);  // 设置key为VK类型,代表变量名
  }
  else  /* ls->t.token == '[' 输入时是'[' */
    yindex(ls, &key);  // 识别 [expr] 结构
  // 此时当前符号应该是=
  cc->nh++;  // 哈希部分的个数应该加1
  checknext(ls, '=');
  rkkey = luaK_exp2RK(fs, &key);  // 得到key在寄存器或常量表中的位置
  expr(ls, &val);  // 读取var
  // 生成OP_SETTABLE指令
  //                              表的位置     key的位置    val的位置
  luaK_codeABC(fs, OP_SETTABLE, cc->t->u.info, rkkey, luaK_exp2RK(fs, &val));
  // key和val占用的寄存器位置不需要了,它们已经设置到表中了
  // 释放key和val占用的寄存器,后面可以继续使用这些寄存器
  fs->freereg = reg;  /* free registers */
}


// 这里处理识别的listfield,将在cc->v中保存的表达式放到寄存器里
// 在constructor函数中每识别一个listfield调用一次这个函数,保存刚刚识别的表达式
static void closelistfield (FuncState *fs, struct ConsControl *cc) {
  if (cc->v.k == VVOID) return;  /* there is no list item */
  // 把cc->v存储的变量放到寄存器里
  luaK_exp2nextreg(fs, &cc->v);
  cc->v.k = VVOID;
  if (cc->tostore == LFIELDS_PER_FLUSH) {  // 到了50个该刷新了 
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);  /* flush */
    cc->tostore = 0;  /* no more items pending 设置完没保存的值就是0了 */
  }
}


// 处理初始化表项中的最后一个listfield
// 为还没有关闭的list域生成SETLIST指令
static void lastlistfield (FuncState *fs, struct ConsControl *cc) {
  if (cc->tostore == 0) return;
  // 表初始化最后一项是函数调用或者...
  // 例如 local a={1,test()}或者 local a={1,...}
  if (hasmultret(cc->v.k)) {
    luaK_setmultret(fs, &cc->v);  // 回填指令
    luaK_setlist(fs, cc->t->u.info, cc->na, LUA_MULTRET);  // OP_SETLIST指令设置为参数个数不定也就是参数C是0
    // 数组部分的个数不记录函数调用或者... 因为他们的个数不确定没法记录
    cc->na--;  /* do not count last expression (unknown number of elements) */
  }
  // 正常情况
  else {
    if (cc->v.k != VVOID)  // 最后一项就是普通的表达式
      luaK_exp2nextreg(fs, &cc->v);  // 存到寄存器中
    luaK_setlist(fs, cc->t->u.info, cc->na, cc->tostore);  // 生成并设置OP_SETLIST指令的参数
  }
}


// listfield是{1,2,3}这种类型不带=
// 该函数直接识别一个表达式写入cc->v
// 让cc->na++;cc->tostore++
static void listfield (LexState *ls, struct ConsControl *cc) {
  /* listfield -> exp */
  expr(ls, &cc->v);  // 识别一个表达式
  // 数组部分的大小超过MAX_INT
  checklimit(ls->fs, cc->na, MAX_INT, "items in a constructor");
  cc->na++;      // 数组部分大小加1
  cc->tostore++; // 等待放入数组部分的值的个数加1
}


// 用来识别表中的每个域
// field分为listfield和recfield
// 就是表的初始化的每一项 例如{1,2,3,a=1}
// 1,2,3属于listfield
// a=1属于recfield
static void field (LexState *ls, struct ConsControl *cc) {
  /* field -> listfield | recfield */
  switch(ls->t.token) {
    // 识别到一个变量名,有可能是listfield也可能是recfield
    case TK_NAME: {  /* may be 'listfield' or 'recfield' */
      // 根据有没有等号决定类型
      if (luaX_lookahead(ls) != '=')  /* expression? 识别TK_NAME后面是不是= */
        listfield(ls, cc);  // {a}这种类型
      else
        recfield(ls, cc);  // {a=1}这种类型
      break;
    }
    case '[': {  // {[1]=1}
      recfield(ls, cc);
      break;
    }
    default: {  // {1,2}
      listfield(ls, cc);
      break;
    }
  }
}


// 识别初始化表的结构 例如{a='1'}
// 开始执行是当前符号是{,结束后当前符号指向}的下一个
// 表达式t是当前识别的表达式在表达式列表中的前一个
// 例如local a,b=1,{1,2} 这里t就表示存储了1的表达式
static void constructor (LexState *ls, expdesc *t) {
  /* constructor -> '{' [ field { sep field } [sep] ] '}'
     sep -> ',' | ';' */
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  int pc = luaK_codeABC(fs, OP_NEWTABLE, 0, 0, 0);
  struct ConsControl cc;
  // 各项数值全部初始化为0
  cc.na = cc.nh = cc.tostore = 0;
  // cc.t保存了最终识别结果
  cc.t = t;
  // 表最终的存储位置还不确定所以设置为VRELOCABLE
  // OP_NEWTABLE的参数A还需要回填,传入pc设置为info保存指令的位置
  init_exp(t, VRELOCABLE, pc);
  // 初始化cc.v,现在还没有值
  init_exp(&cc.v, VVOID, 0);  /* no value (yet) */
  // 将生成的表保存到寄存器中
  luaK_exp2nextreg(ls->fs, t);  /* fix it at stack top */
  checknext(ls, '{');  // 跳过{开始识别 内部结构
  // 循环识别表中的每个域(field)
  do {
    // 除非第一次进来的时候cc.v是VVOID类型,tostore必须大于0
    lua_assert(cc.v.k == VVOID || cc.tostore > 0);
    if (ls->t.token == '}') break;  // 识别到结束位置了
    // 关闭数组部分,数组部分是每识别50个生成一条OP_SETLIST指令
    // 不是一次关闭一个目的是避免每次都生成一条指令提高效率
    // 不等到识别完成再关闭的目的是避免长数组导致寄存器溢出
    closelistfield(fs, &cc);  // 处理一下没有等号的部分,存到寄存器中,如果有必要则生成OP_SETLIST指令
    field(ls, &cc);  // 识别一个项(就是每个被逗号分割的部分)
  } while (testnext(ls, ',') || testnext(ls, ';'));  // 表项之间可以用,;分割
  // 循环过后需要调用lastlistfield关闭剩下还没关闭的域
  check_match(ls, '}', '{', line);  // 跳过最后的}
  lastlistfield(fs, &cc);  // 处理最后识别的表达式
  // 设置OP_NEWTABLE指令的BC参数
  SETARG_B(fs->f->code[pc], luaO_int2fb(cc.na)); /* set initial array size */
  SETARG_C(fs->f->code[pc], luaO_int2fb(cc.nh));  /* set initial table size */
}

/* }====================================================================== */



// 识别定义函数的参数列表 输入时符号是左括号 结束时符号在右括号
// 标记了fs的nactvar,numparams和is_vararg
// 为函数参数准备了寄存器空间
static void parlist (LexState *ls) {
  /* parlist -> [ param { ',' param } ] */
  FuncState *fs = ls->fs;
  Proto *f = fs->f;
  int nparams = 0;
  f->is_vararg = 0;
  // 识别到右括号说明变量识别完了
  if (ls->t.token != ')') {  /* is 'parlist' not empty? */
    // 遍历括号内的内容
    do {
      switch (ls->t.token) {
        case TK_NAME: {  /* param -> NAME 识别到函数参数 */
          // 创建变量 参数个数加1
          new_localvar(ls, str_checkname(ls));
          nparams++;
          break;
        }
        case TK_DOTS: {  /* param -> '...' 识别到三个点 '...'*/
          luaX_next(ls);  // 跳过...
          f->is_vararg = 1;  /* declared vararg 标记函数是可变参数 */
          break;
        }
        default: luaX_syntaxerror(ls, "<name> or '...' expected");
      }
    } while (!f->is_vararg && testnext(ls, ','));
  }
  adjustlocalvars(ls, nparams);
  // 参数个数就是现在的活动的变量个数
  f->numparams = cast_byte(fs->nactvar);
  // 准备出来函数参数的空间
  luaK_reserveregs(fs, fs->nactvar);  /* reserve register for parameters */
}


// 识别函数体 例如 '(t) local a = t+1 end'
// ismethod标记是否是冒号定义,如果是创建self变量
static void body (LexState *ls, expdesc *e, int ismethod, int line) {
  /* body ->  '(' parlist ')' block END */
  // 调用open_func需要新建FuncState,BlockCnt,Proto对象
  FuncState new_fs; // 新建FuncState
  BlockCnt bl;      // 新建BlockCnt
  // 新建Proto对象
  // 这里不能直接调用luaF_newproto因为需要在外部函数(ls->fs)的子函数数组中加入新建项
  new_fs.f = addprototype(ls);
  // 设置行号
  new_fs.f->linedefined = line;
  // 这里实际创建了新函数
  open_func(ls, &new_fs, &bl);
  // 当前必须是左括号 读取下一个符号
  checknext(ls, '(');
  if (ismethod) {  // 标记ismethod 创建self局部变量
    new_localvarliteral(ls, "self");  /* create 'self' parameter */
    adjustlocalvars(ls, 1);
  }
  parlist(ls);  // 识别函数参数
  checknext(ls, ')'); // 识别右括号下一个符号
  statlist(ls);       // 开始识别函数内部的语句
  new_fs.f->lastlinedefined = ls->linenumber;  // 标记函数结束的行号
  check_match(ls, TK_END, TK_FUNCTION, line);
  // 为外部函数生成OP_CLOSURE指令 参数A是freereg
  codeclosure(ls, e);
  // open_func与close_func是一对同时调用
  // 修改ls->fs为原来的值 也就是退出当前函数
  close_func(ls);
}


// 识别表达式列表 用逗号分割
// 返回识别的表达式个数n
// 表达式v被修改为最后识别的表达式,如果有多个表达式前面的表达式结果都被写入寄存器中
static int explist (LexState *ls, expdesc *v) {
  /* explist -> expr { ',' expr } */
  int n = 1;  /* at least one expression */
  expr(ls, v);
  // 这里表达式v被不断复用,一旦能确定后面是逗号就将表达式的值存入寄存器
  // 然后读取新的表达式到v中,这里要确保函数执行完毕v中代表的表达式没有存入寄存器
  // 最后识别的表达式v会由后面调用的函数放入寄存器 例如localstat中调用adjust_assign
  while (testnext(ls, ',')) {
    luaK_exp2nextreg(ls->fs, v);
    expr(ls, v);
    n++;
  }
  return n;
}


// 识别调用函数时传入的参数列表,只在suffixedexp中使用
// lua函数调用支持(),"",{}
// 对于引号和大括号只能传递一个参数,分别也就是他们代表的字符串和表
// 参数f是被调用的函数,现在类型是VNONRELOC,info保存了被调用函数的寄存器位置
static void funcargs (LexState *ls, expdesc *f, int line) {
  FuncState *fs = ls->fs;
  expdesc args;
  int base, nparams;
  // switch语句用来识别调用函数传递的参数
  switch (ls->t.token) {
    // 括号调用,可以传递任何参数
    case '(': {  /* funcargs -> '(' [ explist ] ')' */
      luaX_next(ls);
      if (ls->t.token == ')')  /* arg list is empty? 没有参数 */
        args.k = VVOID;        // 设置args为VVOID类型
      else {
        explist(ls, &args);    // 参数其实就是一个表达式列表
        // 函数调用的参数如果是多返回值就尽量多获取结果
        luaK_setmultret(fs, &args);
      }
      check_match(ls, ')', '(', line);
      break;
    }
    // 大括号调用只能传递它自己代表的表作为参数
    case '{': {  /* funcargs -> constructor */
      constructor(ls, &args);
      break;
    }
    // 引号调用,传入参数就是这个字符串
    case TK_STRING: {  /* funcargs -> STRING */
      codestring(ls, &args, ls->t.seminfo.ts);
      luaX_next(ls);  /* must use 'seminfo' before 'next' */
      break;
    }
    default: {
      luaX_syntaxerror(ls, "function arguments expected");
    }
  }
  // 在switch语句之后还剩下最后一个参数需要保存
  lua_assert(f->k == VNONRELOC);
  // f->u.info现在保存被调用的函数 所以base就是OP_CALL指令的参数A
  base = f->u.info;  /* base register for call */
  // 计算函数的参数个数
  if (hasmultret(args.k))  // 最后一个参数是函数调用或者...说明函数的参数个数不定
    nparams = LUA_MULTRET;  /* open call */
  else {  // 有确定的参数个数
    if (args.k != VVOID)  // VVOID类型不用保存到寄存器上
      luaK_exp2nextreg(fs, &args);  /* close last argument */
    nparams = fs->freereg - (base+1);  // 计算参数个数 freereg是总共几个寄存器 base+1是函数本身和它前面用了几个寄存器
  }
  // 已经保存了被调用的函数和其后的参数,生成实际的函数调用指令
  // 参数A:base是函数自己的位置 
  // 参数B:含义是参数个数+1
  // 参数C:默认有一个返回值
  init_exp(f, VCALL, luaK_codeABC(fs, OP_CALL, base, nparams+1, 2));
  luaK_fixline(fs, line);
  // 函数调用完成就剩下一个返回值
  fs->freereg = base+1;  /* call remove function and arguments and leaves
                            (unless changed) one result */
}




/*
** {======================================================================
** Expression parsing 表达式处理
** =======================================================================
*/


// 识别主表达式 可以是NAME或者括号包起来的表达式
// 只在suffixedexp中使用 识别后缀表达式的前面部分
// 也就是直接一个变量名或者括号包起来的表达式
// a()识别a,(f())()识别(f())
static void primaryexp (LexState *ls, expdesc *v) {
  /* primaryexp -> NAME | '(' expr ')' */
  switch (ls->t.token) {
    case '(': {  // 识别括号内的表达式
      int line = ls->linenumber;
      luaX_next(ls);
      expr(ls, v);
      check_match(ls, ')', '(', line);  // 检查当前符号是)
      luaK_dischargevars(ls->fs, v);
      return;
    }
    case TK_NAME: {
      // 初始化表达式v 可能为VLOCAL,VUPVAL,VINDEXED类型
      singlevar(ls, v);
      return;
    }
    // 既不是变量名也不是左括号报错
    default: {
      luaX_syntaxerror(ls, "unexpected symbol");
    }
  }
}


// 后缀表达式 如a.b a[b] a:b()等等
// primaryexp是就是上面例子的a,后面接上一串的连续调用
// 需要注意的是:xxx不合法,必须是:xxx() 因为使用冒号必须是函数调用
static void suffixedexp (LexState *ls, expdesc *v) {
  /* suffixedexp ->
       primaryexp { '.' NAME | '[' exp ']' | ':' NAME funcargs | funcargs } */
  FuncState *fs = ls->fs;
  int line = ls->linenumber;
  // 先识别后缀表达式的最前面部分
  primaryexp(ls, v);
  // 遍历后缀 所有后缀都是由.[: ("{这六种符号开始
  // 注意后面("{这三种符号都能进行函数调用 a() a"" a{}这三种都行
  for (;;) {
    switch (ls->t.token) {
      case '.': {  /* fieldsel */
        fieldsel(ls, v);
        break;
      }
      case '[': {  /* '[' exp1 ']' */
        expdesc key;
        // 被查询的表放到寄存器或者upvalue中
        // 因为查询表结果可以直接从upvalue中查询,所以不用保存到寄存器中
        luaK_exp2anyregup(fs, v);
        yindex(ls, &key);
        luaK_indexed(fs, v, &key);
        break;
      }
      case ':': {  /* ':' NAME funcargs */
        expdesc key;
        luaX_next(ls);
        checkname(ls, &key);
        luaK_self(fs, v, &key);
        funcargs(ls, v, line);
        break;
      }
      case '(': case TK_STRING: case '{': {  /* funcargs 开始识别函数的参数 */
        // v表示被调用的函数,将该函数保存到寄存器中
        luaK_exp2nextreg(fs, v);
        funcargs(ls, v, line);   // 开始识别函数参数
        break;
      }
      default: return;
    }
  }
}


// 识别简单表达式
static void simpleexp (LexState *ls, expdesc *v) {
  /* simpleexp -> FLT | INT | STRING | NIL | TRUE | FALSE | ... |
                  constructor | FUNCTION body | suffixedexp */
  switch (ls->t.token) {
    case TK_FLT: {  // 浮点类型
      init_exp(v, VKFLT, 0);
      v->u.nval = ls->t.seminfo.r;
      break;
    }
    case TK_INT: {  // 整数类型
      init_exp(v, VKINT, 0);
      v->u.ival = ls->t.seminfo.i;
      break;
    }
    case TK_STRING: {  // 字符串类型
      // 识别字符串直接存入常量表,设置表达式类型为VK
      codestring(ls, v, ls->t.seminfo.ts);
      break;
    }
    case TK_NIL: {  // nil对象
      init_exp(v, VNIL, 0);
      break;
    }
    case TK_TRUE: {  // true
      init_exp(v, VTRUE, 0);
      break;
    }
    case TK_FALSE: {  // false
      init_exp(v, VFALSE, 0);
      break;
    }
    case TK_DOTS: {  /* vararg ... 识别到可变参数 */
      FuncState *fs = ls->fs;
      check_condition(ls, fs->f->is_vararg,
                      "cannot use '...' outside a vararg function");
      // 设置表达式v 生成OP_VARARG指令 参数AB都需要回填 参数B先设置成1
      init_exp(v, VVARARG, luaK_codeABC(fs, OP_VARARG, 0, 1, 0));
      break;
    }
    // 剩下三种情况使用return是因为他们都已经读取了下一个符号 不需要执行luaX_next
    case '{': {  /* constructor 识别表初始化结构 */
      constructor(ls, v);
      return;
    }
    case TK_FUNCTION: {  // 识别函数
      luaX_next(ls);  // 跳过function关键字
      body(ls, v, 0, ls->linenumber);
      return;
    }
    default: {
      suffixedexp(ls, v);
      return;
    }
  }
  luaX_next(ls);
}


// 从int类型转换成UnOpr枚举类型
static UnOpr getunopr (int op) {
  switch (op) {
    case TK_NOT: return OPR_NOT;
    case '-': return OPR_MINUS;
    case '~': return OPR_BNOT;
    case '#': return OPR_LEN;
    default: return OPR_NOUNOPR;
  }
}


// 从int类型转换成BinOpr枚举类型
static BinOpr getbinopr (int op) {
  switch (op) {
    case '+': return OPR_ADD;
    case '-': return OPR_SUB;
    case '*': return OPR_MUL;
    case '%': return OPR_MOD;
    case '^': return OPR_POW;
    case '/': return OPR_DIV;
    case TK_IDIV: return OPR_IDIV;
    case '&': return OPR_BAND;
    case '|': return OPR_BOR;
    case '~': return OPR_BXOR;
    case TK_SHL: return OPR_SHL;
    case TK_SHR: return OPR_SHR;
    case TK_CONCAT: return OPR_CONCAT;
    case TK_NE: return OPR_NE;
    case TK_EQ: return OPR_EQ;
    case '<': return OPR_LT;
    case TK_LE: return OPR_LE;
    case '>': return OPR_GT;
    case TK_GE: return OPR_GE;
    case TK_AND: return OPR_AND;
    case TK_OR: return OPR_OR;
    default: return OPR_NOBINOPR;
  }
}


// 定义了各个运算符的左右优先级
// 越大优先级越高
static const struct {
  lu_byte left;  /* left priority for each binary operator */
  lu_byte right; /* right priority */
} priority[] = {  /* ORDER OPR */
   {10, 10}, {10, 10},           /* '+' '-' */
   {11, 11}, {11, 11},           /* '*' '%' */
   {14, 13},                  /* '^' (right associative) */
   {11, 11}, {11, 11},           /* '/' '//' */
   {6, 6}, {4, 4}, {5, 5},   /* '&' '|' '~' */
   {7, 7}, {7, 7},           /* '<<' '>>' */
   // ..是右结合,说明 1 .. 2 .. 3是1 ..(2 .. 3)
   {9, 8},                   /* '..' (right associative) */
   {3, 3}, {3, 3}, {3, 3},   /* ==, <, <= */
   {3, 3}, {3, 3}, {3, 3},   /* ~=, >, >= */
   {2, 2}, {1, 1}            /* and, or */
};

#define UNARY_PRIORITY	12  /* priority for unary operators 一元操作符的优先级 */


/*
** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
** where 'binop' is any binary operator with a priority higher than 'limit'
*/
// 识别subexpr结构 binop是二元操作符
// binop的优先级必须要比输入的limit要高
// subexpr是通过多个一元和二元操作符连接后的simpleexp
// 调用过程解析:识别local a=1<2 and 3>2 or 0>0 and 1<2
// 最终生成结果,常量表的内容是 -1:1,-2:2,-3:3,-4:0
// Ln代表第几条指令,例如L1代表第一条指令OP_LT
// 1	LT       	0 -1 -2	; 1 2
// 2	JMP      	0 2	; to 5
// 3	LT       	1 -2 -3	; 2 3
// 4	JMP      	0 5	; to 10
// 5	LT       	0 -4 -4	; 0 0
// 6	JMP      	0 2	; to 9
// 7	LT       	1 -1 -2	; 1 2
// 8	JMP      	0 1	; to 10
// 9	LOADBOOL 	0 0 1
// 10	LOADBOOL 	0 1 0
// 先识别了1为v1 VINT类型,再识别操作符<,进入while循环
// 针对<操作符,infix函数将1存入RK中这里的话一定是常量表
// 再调用subexpr根据<的优先级 读取2到v2返回操作符and
// 输入v1,v2和操作符<,在posfix函数中生成L1,L2,并修改e1为VJMP类型
// 进入下一轮while,操作符是and,v1类型为VJMP,共有2条指令
// infix函数根据and操作,将L1参数A取反,L2加入v1的falselist中
// 继续调用subexpr识别第二个操作符,根据上面的分析这次识别了3>2 返回or操作符
//   识别过程中还生成了L3和L4,设置v2为VJMP类型
// 进入posfix函数,v1,v2都是VJMP,操作符是and 合并v1,v2的falselist v1被修改为v2,也就是info指向了L4
// 执行了两轮while循环后 op为or,v1为VJMP类型,v1->u.info指向L4,falselist里是L1,truelist为空
// 进入infix函数,针对or操作符 L4加入v1的truelist,v1的falselist也就是L1加入fs->jpc后被清空
// 调用subexpr识别第二个操作数,这次识别了0>0 and 1<2
//   v2为VJMP,生成了L5L6L7L8四条指令,truelist为空,falselist指向L6
//   除此之外,因为调用subexpr时fs->jpc中有L2,所以在生成L5时回填了L2让它跳转到了L5
// 现在v1,v2都是VJMP,v1的truelist有L4,v2的falselist有L6
// 接下来输入操作符or,调用posfix函数 最终使v1的truelist为L4,falselist为L6
// 最终调用expr函数生成的指令和参数如下,表达式v为VJMP类型 truelist为L4,falselist为L6
//   1	LT       	0 -1 -2	; 1 2
//   2	JMP      	0 2	; to 5
//   3	LT       	1 -2 -3	; 2 3
//   4	JMP      	0 -1	; to 4
//   5	LT       	0 -4 -4	; 0 0
//   6	JMP      	0 -1	; to 6
//   7	LT       	1 -1 -2	; 1 2
//   8	JMP      	0 -1	; to 8
// 接下来回到localstat函数调用adjust_assign,在这里要把表达式存入寄存器最终调用exp2reg
//   将L8加入了truelist,生成两条OP_LOADBOOL指令 并用它们回填truelist和falselist
static BinOpr subexpr (LexState *ls, expdesc *v, int limit) {
  BinOpr op;      // 二元操作符
  UnOpr uop;      // 一元操作符
  enterlevel(ls); // ls->L->nCcalls++
  uop = getunopr(ls->t.token);  // 识别出来一元运算符
  if (uop != OPR_NOUNOPR) {  // 有一元运算符
    int line = ls->linenumber;
    luaX_next(ls);
    // 一元运算符的优先级是11 只有^进行指数运算优先级高于它
    subexpr(ls, v, UNARY_PRIORITY);
    // 执行后v要么保存最后结果,要么生成了计算指令变成VRELOCABLE类型
    luaK_prefix(ls->fs, uop, v, line);
  }
  // 没有一元操作符识别出来一个simpleexp
  else simpleexp(ls, v);
  /* expand while operators have priorities higher than 'limit' */
  // 读取当前的二元操作符
  op = getbinopr(ls->t.token);
  // 现在已经读取了第一个操作数和操作符
  // 如果操作符是OPR_NOBINOPR说明表达式已经结束了,跳过循环直接返回
  // 如果识别的符号的左优先级小于等于上一层递归的符号的右优先级,也应该跳过循环
  // 例如 1+1<2,识别到<了,<的左优先级是3,传入的limit是+的右优先级10
  // 所以这里只识别1 <2的部分应该到递归的外部函数去处理
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    expdesc v2;
    BinOpr nextop;
    int line = ls->linenumber;
    luaX_next(ls);
    luaK_infix(ls->fs, op, v);  // 根据操作符设置第一个操作数
    /* read sub-expression with higher priority */
    // 读取第二个操作数
    // 第二个操作数的界限是识别到一个操作符的左优先级小于等于当前操作符的右优先级
    // 例如解析'1 + 1 * 2 and 3', 当前识别了'1 +'
    // 即将执行下一行,也就是subexpr(ls,&v2,10)因为加号的右优先级是10
    // *的左优先级是11大于10,and的左优先级是2小于10 所以这里返回and符号
    // 也就是识别了第二个操作数为'1 * 2'
    nextop = subexpr(ls, &v2, priority[op].right);
    // 根据两个操作数和操作符生成指令
    luaK_posfix(ls->fs, op, v, &v2, line);
    op = nextop;
  }
  leavelevel(ls);
  return op;  /* return first untreated operator */
}


static void expr (LexState *ls, expdesc *v) {
  subexpr(ls, v, 0);
}

/* }==================================================================== */



/*
** {======================================================================
** Rules for Statements
** =======================================================================
*/


// 识别一个非循环块,实质就是调用了statlist
// 该函数调用了enterblock,leaveblock
static void block (LexState *ls) {
  /* block -> statlist */
  FuncState *fs = ls->fs;
  BlockCnt bl;
  enterblock(fs, &bl, 0);  // 传入0代表不是循环块
  statlist(ls);
  leaveblock(fs);
}


/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
// LHS left-hand side
// 保存赋值语句的左半部分,用一个链串起来
struct LHS_assign {
  // 前一个被赋值的变量
  struct LHS_assign *prev;
  // 被赋值的变量的描述信息
  expdesc v;  /* variable (global, local, upvalue, or indexed) */
};


/*
** check whether, in an assignment to an upvalue/local variable, the
** upvalue/local variable is begin used in a previous assignment to a
** table. If so, save original upvalue/local value in a safe place and
** use this safe copy in the previous assignment.
*/
// 因为赋值语句是逆序赋值的,要防止两种情况赋值时的覆盖情况
// 一种是a[b],a=1,2 覆盖表
// 一种是a[b],b=1,2 覆盖键
// lh是被赋值变量链表,v是刚识别出来的变量还没有链上lh
static void check_conflict (LexState *ls, struct LHS_assign *lh, expdesc *v) {
  FuncState *fs = ls->fs;
  int extra = fs->freereg;  /* eventual position to save local variable */
  int conflict = 0;
  // 遍历lh
  for (; lh; lh = lh->prev) {  /* check all previous assignments */
    // 如果之前的变量要为表项赋值
    if (lh->v.k == VINDEXED) {  /* assigning to a table? */
      /* table is the upvalue/local being assigned now? */
      // 前面赋值的表项的表现在要被赋值例如 a[b],a=1,2
      // 前面的a[b]使用拷贝的a 避免冲突
      if (lh->v.u.ind.vt == v->k && lh->v.u.ind.t == v->u.info) {
        conflict = 1;  // 标记冲突
        lh->v.u.ind.vt = VLOCAL;  // 代表表被拷贝到寄存器中
        lh->v.u.ind.t = extra;  /* previous assignment will use safe copy 表被拷贝到extra位置 */
      }
      /* index is the local being assigned? (index cannot be upvalue) */
      // 前面赋值的表项的key现在要被赋值例如 a[b],b=1,2
      // 如果b是upvalue那么前面a[b]中使用的b已经加载到寄存器了,不存在覆盖的问题
      // 将b拷贝到extra前面的a[b]使用拷贝的b
      if (v->k == VLOCAL && lh->v.u.ind.idx == v->u.info) {
        conflict = 1;
        lh->v.u.ind.idx = extra;  /* previous assignment will use safe copy */
      }
    }
  }
  if (conflict) {
    /* copy upvalue/local value to a temporary (in position 'extra') */
    // 如果冲突 被查询的表可能是upvalue或者在寄存器中,key只可能是在寄存器
    OpCode op = (v->k == VLOCAL) ? OP_MOVE : OP_GETUPVAL;
    luaK_codeABC(fs, op, extra, v->u.info, 0);
    luaK_reserveregs(fs, 1);
  }
}


// 识别多个 ', suffixedexp' 结构 以及=后面的explist表达式列表
// nvars代表当前已经识别出的赋值语句前面的变量个数
static void assignment (LexState *ls, struct LHS_assign *lh, int nvars) {
  expdesc e;
  check_condition(ls, vkisvar(lh->v.k), "syntax error");
  // 现在是等号前面
  if (testnext(ls, ',')) {  /* assignment -> ',' suffixedexp assignment */
    struct LHS_assign nv;
    // 在LHS_assign链表中新增一个节点
    nv.prev = lh;
    suffixedexp(ls, &nv.v);
    if (nv.v.k != VINDEXED)
      check_conflict(ls, lh, &nv.v);
    checklimit(ls->fs, nvars + ls->L->nCcalls, LUAI_MAXCCALLS,
                    "C levels");
    // 递归识别suffixedexp
    assignment(ls, &nv, nvars+1);
  }
  // 开始识别等号后面
  else {  /* assignment -> '=' explist */
    int nexps;
    checknext(ls, '=');  // 跳过=
    // 识别表达式列表
    nexps = explist(ls, &e);
    if (nexps != nvars)
      adjust_assign(ls, nvars, nexps, &e);
    else {
      luaK_setoneret(ls->fs, &e);  /* close last expression */
      luaK_storevar(ls->fs, &lh->v, &e);
      return;  /* avoid default */
    }
  }
  // 每退出一层递归倒序向前赋值一个值,所有结果都通过explist保存在了寄存器中
  init_exp(&e, VNONRELOC, ls->fs->freereg-1);  /* default assignment */
  luaK_storevar(ls->fs, &lh->v, &e);
}


// 识别一个条件表达式
static int cond (LexState *ls) {
  /* cond -> exp */
  expdesc v;
  expr(ls, &v);  /* read condition */
  if (v.k == VNIL) v.k = VFALSE;  /* 'falses' are all equal here */
  luaK_goiftrue(ls->fs, &v);
  return v.f;
}


// 识别goto语句和break语句
// lua的break语句本质上就是一个goto语句
// pc是一条跳转指令,该函数就是为了回填这条跳转指令的目标位置
// 如果goto对应的label已经识别过,那么就会回填成功,否则将goto加入到gotolist(ls->dyd->gt)中
static void gotostat (LexState *ls, int pc) {
  int line = ls->linenumber;
  TString *label;
  int g;
  // 这里当前符号可能是goto或者break
  if (testnext(ls, TK_GOTO))
    label = str_checkname(ls);
  else {
    luaX_next(ls);  /* skip break */
    // 为break语句创建break字符串
    label = luaS_new(ls->L, "break");
  }
  // 向goto列表中加入新的项
  g = newlabelentry(ls, &ls->dyd->gt, label, line, pc);
  findlabel(ls, g);  /* close it if label already defined */
}


/* check for repeated labels on the same block */
// 检查在当前块内是否有重复的label
// 遍历Labellist,比较label的名称有没有重复的即可,有重复的直接报错
static void checkrepeated (FuncState *fs, Labellist *ll, TString *label) {
  int i;
  // 遍历当前块内的所有label
  for (i = fs->bl->firstlabel; i < ll->n; i++) {
    if (eqstr(label, ll->arr[i].name)) {
      const char *msg = luaO_pushfstring(fs->ls->L,
                          "label '%s' already defined on line %d",
                          getstr(label), ll->arr[i].line);
      semerror(fs->ls, msg);
    }
  }
}


/* skip no-op statements */
// 跳过无操作语句
// 无操作语句只有两种,分别是只有一个分号(;)和label语句(::label::)
// 当前符号是分号或双冒号则调用statement函数识别一条语句
static void skipnoopstat (LexState *ls) {
  while (ls->t.token == ';' || ls->t.token == TK_DBCOLON)
    statement(ls);
}


// 识别一个lable
// 当前符号指向第二个双冒号,label是标签名
// 有关label的信息存放在ls->dyd->label中
// 识别label语句后尝试在当前块的待回填goto语句中查询并回填
static void labelstat (LexState *ls, TString *label, int line) {
  /* label -> '::' NAME '::' */
  FuncState *fs = ls->fs;
  Labellist *ll = &ls->dyd->label;
  int l;  /* index of new label being created */
  // 检查要定义的标签在当前块内是否重名
  checkrepeated(fs, ll, label);  /* check for repeated labels */
  // 跳过第二个双冒号
  checknext(ls, TK_DBCOLON);  /* skip double colon */
  /* create new entry for this label */
  // 增加新label的记录,保存在ls->dyd->label中
  // 这里传入的pc就是当前的pc,因为还不知道label对应的goto的指令是多少
  l = newlabelentry(ls, ll, label, line, luaK_getlabel(fs));
  skipnoopstat(ls);  /* skip other no-op statements */
  // 如果一个标签语句是当前块的最后一句或者后面只有无操作语句
  // 那么任意可见这个label的goto语句都可以跳转到这里,不考虑其他变量作用域的问题
  // goto l
  // local a,b,c=1,2,3
  // ::l::
  // 这三句就不会报错,如果在::l::后面加上任何有操作语句就会报错
  // block_follow传入0的原因是repeat...until循环块结束后变量作用域并没有结束,不能不考虑作用域问题
  if (block_follow(ls, 0)) {  /* label is last no-op statement in the block? */
    /* assume that locals are already out of scope */
    ll->arr[l].nactvar = fs->bl->nactvar;
  }
  // 在当前块内寻找匹配的goto,回填goto的跳转指令
  // 语法分析是一遍完成的,如果先定义了goto语句,必须在这次label语句回填目标位置
  // 每次识别label和识别goto都会查询对应的语句
  findgotos(ls, &ll->arr[l]);
}


// 识别while语句
static void whilestat (LexState *ls, int line) {
  /* whilestat -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  int whileinit;
  int condexit;
  BlockCnt bl;
  // 跳过while
  luaX_next(ls);  /* skip WHILE */
  // whileinit就是当前的pc,也就是while的条件表达式的第一条指令
  whileinit = luaK_getlabel(fs);
  // condexit是表达式的falselist, 跳转到while语句的结束位置
  // truelist已经使用了下一条指令回填,也就是while的block的第一条指令
  condexit = cond(ls);
  // 进入块, 1标记这是循环块
  enterblock(fs, &bl, 1);
  // 跳过do
  checknext(ls, TK_DO);
  // 识别block
  block(ls);
  // 生成一条跳转到while语句开始位置的OP_JMP指令
  luaK_jumpto(fs, whileinit);
  // 识别end
  check_match(ls, TK_END, TK_WHILE, line);
  // 退出当前块
  leaveblock(fs);
  // while语句识别完毕, 条件表达式的falselist使用下一条指令回填
  luaK_patchtohere(fs, condexit);  /* false conditions finish the loop */
}


// 识别repeat语句
static void repeatstat (LexState *ls, int line) {
  /* repeatstat -> REPEAT block UNTIL cond */
  int condexit;
  FuncState *fs = ls->fs;
  int repeat_init = luaK_getlabel(fs);
  BlockCnt bl1, bl2;
  enterblock(fs, &bl1, 1);  /* loop block */
  enterblock(fs, &bl2, 0);  /* scope block */
  luaX_next(ls);  /* skip REPEAT */
  statlist(ls);
  check_match(ls, TK_UNTIL, TK_REPEAT, line);
  // 识别条件表达式, 返回falselist
  // 表达式为真直接就执行下一条语句的指令
  // falselist使用repeat语句开始位置回填
  condexit = cond(ls);  /* read condition (inside scope block) */
  // bl2块内定义了其内部块的upvalue
  if (bl2.upval)  /* upvalues? */
    // 回填JMP指令的参数A
    luaK_patchclose(fs, condexit, bl2.nactvar);
  leaveblock(fs);  /* finish scope */
  // 条件表达式为假的时候跳转到repeat_init位置
  luaK_patchlist(fs, condexit, repeat_init);  /* close the loop */
  leaveblock(fs);  /* finish loop */
}


// 读取一个表达式并写入寄存器
static int exp1 (LexState *ls) {
  expdesc e;
  int reg;
  expr(ls, &e);
  luaK_exp2nextreg(ls->fs, &e);
  lua_assert(e.k == VNONRELOC);
  reg = e.u.info;
  return reg;
}


// 解析for循环体的语句
// base是for循环开始使用的寄存器位置
// nvars是定义的额外变量的个数
// isnum为1标记是第一种for循环,为0标记是第二种for循环
static void forbody (LexState *ls, int base, int line, int nvars, int isnum) {
  /* forbody -> DO block */
  BlockCnt bl;
  FuncState *fs = ls->fs;
  int prep, endfor;
  // 两种for循环都有三个伪变量,由new_localvarliteral定义
  adjustlocalvars(ls, 3);  /* control variables */
  checknext(ls, TK_DO);
  // 第一种for循环 循环体包裹在OP_FORPREP和OP_FORLOOP之间
  // 第二种for循环 循环体包裹在OP_JMP和连续的OP_TFORCALL与OP_TFORLOOP之间
  // prep可能是OP_FORPREP指令或者OP_JMP指令,都是A sBx类型指令,两者的sBx参数含义一致,都是跳转的目标位置
  prep = isnum ? luaK_codeAsBx(fs, OP_FORPREP, base, NO_JUMP) : luaK_jump(fs);
  enterblock(fs, &bl, 0);  /* scope for declared variables */
  // 根据传入的nvars调整额外定义的变量,额外变量的作用域在被包裹的区域内
  adjustlocalvars(ls, nvars);
  luaK_reserveregs(fs, nvars);
  block(ls);
  leaveblock(fs);  /* end of scope for declared variables */
  // for循环包裹块的前部指令OP_FORPREP或者OP_JMP都需要跳转到后部
  luaK_patchtohere(fs, prep);
  // 第一种由OP_FORLOOP结束
  if (isnum)  /* numeric for? */
    endfor = luaK_codeAsBx(fs, OP_FORLOOP, base, NO_JUMP);
  // 第二种由OP_TFORCALL和OP_TFORLOOP结束
  else {  /* generic for */
    luaK_codeABC(fs, OP_TFORCALL, base, 0, nvars);
    // 设置OP_TAILCALL的行位置
    luaK_fixline(fs, line);
    endfor = luaK_codeAsBx(fs, OP_TFORLOOP, base + 2, NO_JUMP);
  }
  // 两种类型后部指令都需要跳转到前部指令的下一条指令
  luaK_patchlist(fs, endfor, prep + 1);
  // 生成尾部指令的行是for循环的开始位置
  luaK_fixline(fs, line);
}


// 解析第一种for循环
// for var=exp1,exp2,exp3 do  
//   <执行体>  
// end  
static void fornum (LexState *ls, TString *varname, int line) {
  /* fornum -> NAME = exp1,exp1[,exp1] forbody */
  FuncState *fs = ls->fs;
  int base = fs->freereg;
  // 生成三个循环的伪变量
  new_localvarliteral(ls, "(for index)");
  new_localvarliteral(ls, "(for limit)");
  new_localvarliteral(ls, "(for step)");
  // 生成循环变量
  new_localvar(ls, varname);
  checknext(ls, '=');
  // 解析初始值到寄存器中
  exp1(ls);  /* initial value */
  checknext(ls, ',');
  // 解析限制值到寄存器中
  exp1(ls);  /* limit */
  // 解析步长到寄存器,步长是可选值,默认是1
  if (testnext(ls, ','))  // 有步长直接解析
    exp1(ls);  /* optional step */
  else {  /* default step = 1 没有步长设置默认值1 */
    // 将常量1放入寄存器
    luaK_codek(fs, fs->freereg, luaK_intK(fs, 1));
    luaK_reserveregs(fs, 1);
  }
  // 这种类型的for循环定义了一个额外变量
  // var=exp1定义的变量var
  forbody(ls, base, line, 1, 1);
}


// 解析第二种for循环
// for i, v in ipairs(a) do
//  print(i, v)
// end 
static void forlist (LexState *ls, TString *indexname) {
  /* forlist -> NAME {,NAME} IN explist forbody */
  FuncState *fs = ls->fs;
  expdesc e;
  int nvars = 4;  /* gen, state, control, plus at least one declared var */
  int line;
  int base = fs->freereg;
  /* create control variables */
  new_localvarliteral(ls, "(for generator)");
  new_localvarliteral(ls, "(for state)");
  new_localvarliteral(ls, "(for control)");
  /* create declared variables */
  // 额外变量的个数不确定,可能是任意个
  new_localvar(ls, indexname);
  while (testnext(ls, ',')) {
    new_localvar(ls, str_checkname(ls));
    nvars++;
  }
  checknext(ls, TK_IN);
  line = ls->linenumber;
  // 为三个伪变量赋值
  adjust_assign(ls, 3, explist(ls, &e), &e);
  luaK_checkstack(fs, 3);  /* extra space to call generator */
  forbody(ls, base, line, nvars - 3, 0);
}


// 识别for语句
// for语句分为两种模式
// 第一种调用fornum
// 第二种调用forlist
static void forstat (LexState *ls, int line) {
  /* forstat -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  TString *varname;
  BlockCnt bl;
  enterblock(fs, &bl, 1);  /* scope for loop and control variables */
  luaX_next(ls);  /* skip 'for' */
  varname = str_checkname(ls);  /* first variable name */
  // 第一种
  // for var=exp1,exp2,exp3 do  
  //   <执行体>  
  // end  
  // var从exp1变化到exp2,每次变化以exp3为步长递增var,并执行一次"执行体"
  // exp3是可选的,如果不指定,默认为1
  // 第二种
  // for i, v in ipairs(a) do
  //  print(i, v)
  // end 
  switch (ls->t.token) {
    // 识别第一种
    case '=': fornum(ls, varname, line); break;
    // 识别第二种
    case ',': case TK_IN: forlist(ls, varname); break;
    default: luaX_syntaxerror(ls, "'=' or 'in' expected");
  }
  check_match(ls, TK_END, TK_FOR, line);
  leaveblock(fs);  /* loop scope ('break' jumps to this point) */
}


// 识别 '[IF|ELSEIF] cond THEN block' 结构 注意不包括else块
static void test_then_block (LexState *ls, int *escapelist) {
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  BlockCnt bl;
  FuncState *fs = ls->fs;
  expdesc v;  // 用于保存条件表达式
  // 条件为假的时候跳转位置,显然是识别完当前块的下一条指令
  int jf;  /* instruction to skip 'then' code (if condition is false) */
  luaX_next(ls);  /* skip IF or ELSEIF 跳过if或者elseif */
  expr(ls, &v);  /* read condition 识别需要if判断的条件表达式 */
  checknext(ls, TK_THEN);  // 跳过then 识别下一个符号
  // 特殊情况 识别到goto和break
  if (ls->t.token == TK_GOTO || ls->t.token == TK_BREAK) {
    luaK_goiffalse(ls->fs, &v);  /* will jump to label if condition is true */
    enterblock(fs, &bl, 0);  /* must enter block before 'goto' */
    // 表达式为真,直接跳转到标签位置
    gotostat(ls, v.t);  /* handle goto/break */
    skipnoopstat(ls);  /* skip other no-op statements */
    // 如果当前块就只有一条goto语句,直接返回就行了
    // 表达式为假直接执行下一块的语句
    if (block_follow(ls, 0)) {  /* 'goto' is the entire block? */
      leaveblock(fs);
      return;  /* and that is it */
    }
    else  /* must skip over 'then' part if condition is false */
      // 表达式为假执行这条跳转指令,跳转到当前块结束
      jf = luaK_jump(fs);
  }
  // 处理正常的if,进去不直接是goto或者break
  else {  /* regular case (not goto/break) */
    // goiftrue的意思如果表达式v为真继续执行,表达式v为假那么需要执行跳转指令跳转
    luaK_goiftrue(ls->fs, &v);  /* skip over block if condition is false */
    enterblock(fs, &bl, 0);
    jf = v.f;  // 保存falselist,解析完这个块内的语句后回填这个链
  }
  // 识别块内的语句
  statlist(ls);  /* 'then' part */
  leaveblock(fs);  // 在上面的if判断中执行了enterblock
  // 执行完当前if或elseif块之后需要跳转到整个if语句结束位置
  // 所以如果下面还有else或者elseif那么就生成一条跳转指令
  // 这条跳转指令跳转到if语句的下一条指令,这里还不知道if语句的结束位置,所以加入escapelist
  if (ls->t.token == TK_ELSE ||
      ls->t.token == TK_ELSEIF)  /* followed by 'else'/'elseif'? */
    luaK_concat(fs, escapelist, luaK_jump(fs));  /* must jump over it */
  // 已经解析完这个块的语句了,下一条语句就是falselist中要回填的位置
  luaK_patchtohere(fs, jf);
}


// 处理if语句 当前符号指向if
// if语句包括1个if块,多个或0个elseif块,1个或0个else块
static void ifstat (LexState *ls, int line) {
  /* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
  FuncState *fs = ls->fs;
  // 一条记录所有需要跳转到if语句结束后第一条指令的跳转指令的回填链
  int escapelist = NO_JUMP;  /* exit list for finished parts */
  // 识别第一个if块
  test_then_block(ls, &escapelist);  /* IF cond THEN block */
  while (ls->t.token == TK_ELSEIF)  // 识别每个elseif块
    test_then_block(ls, &escapelist);  /* ELSEIF cond THEN block */
  if (testnext(ls, TK_ELSE))  // 识别可能存在的else块
    block(ls);  /* 'else' part */
  check_match(ls, TK_END, TK_IF, line);
  // if语句已经结束,下一条语句的位置就是escapelist回填的位置,将escapelist加入fs->jpc
  luaK_patchtohere(fs, escapelist);  /* patch escape list to 'if' end */
}


// 识别定义局部函数
static void localfunc (LexState *ls) {
  expdesc b;
  FuncState *fs = ls->fs;
  // 根据函数名创建局部变量 创建成功读取下一个符号左括号
  new_localvar(ls, str_checkname(ls));  /* new local variable */
  // 增加局部变量的个数 为这个函数局部变量设置startpc
  adjustlocalvars(ls, 1);  /* enter its scope */
  // 识别函数体 例如 '(t) local a = t+1 end'
  body(ls, &b, 0, ls->linenumber);  /* function created in next register */
  /* debug information will only see the variable after this point! */
  // 设置这个局部变量(函数)开始作用的位置
  getlocvar(fs, b.u.info)->startpc = fs->pc;
}


// 调用时ls->t已经跳过local 读入了第一个变量名
static void localstat (LexState *ls) {
  /* stat -> LOCAL NAME {',' NAME} ['=' explist] */
  int nvars = 0;  // 变量个数
  int nexps;      // 表达式个数
  expdesc e;
  // 识别赋值语句等号前面的变量名称
  do {
    new_localvar(ls, str_checkname(ls));
    nvars++;
  } while (testnext(ls, ','));  // 变量名后面应该是,
  if (testnext(ls, '='))        // 识别完名字该是=了
    nexps = explist(ls, &e);
  else {  // 没有等号
    e.k = VVOID;
    nexps = 0;
  }
  adjust_assign(ls, nvars, nexps, &e);  // 调整变量和表达式的个数
  // 设置fs的nactvar局部变量个数 设置局部变量的statpc属性
  adjustlocalvars(ls, nvars);
}


// 识别函数名
// 返回值为1说明这是一个对象方法定义或调用,也就是最后是冒号,例如a.a:a
// 返回值为0说明不带冒号
static int funcname (LexState *ls, expdesc *v) {
  /* funcname -> NAME {fieldsel} [':' NAME] */
  int ismethod = 0;
  singlevar(ls, v);  // 读取一个变量名
  // 函数名可以是 a.a.a:b
  // 冒号必须在最后且只能有一个
  while (ls->t.token == '.')
    fieldsel(ls, v);
  // 最后一个部分是冒号
  if (ls->t.token == ':') {
    ismethod = 1;
    fieldsel(ls, v);
  }
  return ismethod;
}


// 识别一个函数声明
// 输入时当前符号是function
static void funcstat (LexState *ls, int line) {
  /* funcstat -> FUNCTION funcname body */
  int ismethod;
  expdesc v, b;
  luaX_next(ls);  /* skip FUNCTION */
  // 表达式v代表要被赋值的变量
  ismethod = funcname(ls, &v);
  // 表达式b用来保存生成的函数的位置
  body(ls, &b, ismethod, line);
  // 将函数进行赋值
  luaK_storevar(ls->fs, &v, &b);
  luaK_fixline(ls->fs, line);  /* definition "happens" in the first line */
}


// 单独函数调用或者赋值语句 a()或者a=1这两种
static void exprstat (LexState *ls) {
  /* stat -> func | assignment */
  FuncState *fs = ls->fs;
  struct LHS_assign v;
  // 先识别一个后缀表达式
  suffixedexp(ls, &v.v);
  // 处理赋值语句
  if (ls->t.token == '=' || ls->t.token == ',') { /* stat -> assignment ? */
    v.prev = NULL;         // 这个是LHS_assign链表的头节点
    assignment(ls, &v, 1); // 继续识别剩下的部分 已经识别了1个所以nvars是1
  }
  // 处理单独的函数调用
  else {  /* stat -> func */
    check_condition(ls, v.v.k == VCALL, "syntax error");
    // 单独的函数调用显然不用保存返回值
    // OP_CALL指令的参数C被funcargs默认为2
    // 这里修改为1 代表不保存返回值
    SETARG_C(getinstruction(fs, &v.v), 1);  /* call statement uses no results */
  }
}


static void retstat (LexState *ls) {
  /* stat -> RETURN [explist] [';'] */
  FuncState *fs = ls->fs;
  expdesc e;
  int first, nret;  /* registers with returned values */
  // return后面直接就是分号或者后面就是新块 直接结束
  if (block_follow(ls, 1) || ls->t.token == ';')
    first = nret = 0;  /* return no values 不返回任何值 */
  else {
    // nret现在是表达式个数,如果是多返回值表达式nret需要设置为LUA_MULTRET
    nret = explist(ls, &e);  /* optional return values 解析表达式列表 */
    // 区分返回值是不是多返回值
    if (hasmultret(e.k)) {
      luaK_setmultret(fs, &e);
      // 例如return f(),尾调用单独优化
      if (e.k == VCALL && nret == 1) {  /* tail call? */
        SET_OPCODE(getinstruction(fs,&e), OP_TAILCALL);
        lua_assert(GETARG_A(getinstruction(fs,&e)) == fs->nactvar);
      }
      first = fs->nactvar;
      nret = LUA_MULTRET;  /* return all values */
    }
    else {
      // 这里进行了优化
      // 如果只有一个返回值,这个值可能已经在寄存器里了直接返回这个位置
      // 如果有多个返回值,只能放在连续的放在的最后几个使用的寄存器
      if (nret == 1)  /* only one single value? */
        first = luaK_exp2anyreg(fs, &e);
      else {
        luaK_exp2nextreg(fs, &e);  /* values must go to the stack */
        first = fs->nactvar;  /* return all active values */
        lua_assert(nret == fs->freereg - first);
      }
    }
  }
  // 生成最后的OP_RETURN指令
  luaK_ret(fs, first, nret);
  // 跳过return语句最后可能的分号
  testnext(ls, ';');  /* skip optional semicolon */
}


// 识别一条语句
static void statement (LexState *ls) {
  // statement在调用时fs->nactvar==fs->freereg
  // 保存当前行 可能会被报错信息使用
  int line = ls->linenumber;  /* may be needed for error messages */
  enterlevel(ls);
  switch (ls->t.token) {
    // 语句开始直接就是分号跳过
    case ';': {  /* stat -> ';' (empty statement) */
      luaX_next(ls);  /* skip ';' */
      break;
    }
    // 处理if语句
    case TK_IF: {  /* stat -> ifstat */
      ifstat(ls, line);
      break;
    }
    case TK_WHILE: {  /* stat -> whilestat */
      whilestat(ls, line);
      break;
    }
    case TK_DO: {  /* stat -> DO block END */
      luaX_next(ls);  /* skip DO */
      block(ls);
      check_match(ls, TK_END, TK_DO, line);
      break;
    }
    case TK_FOR: {  /* stat -> forstat */
      forstat(ls, line);
      break;
    }
    case TK_REPEAT: {  /* stat -> repeatstat */
      repeatstat(ls, line);
      break;
    }
    case TK_FUNCTION: {  /* stat -> funcstat */
      funcstat(ls, line);
      break;
    }
    case TK_LOCAL: {  /* stat -> localstat 识别定义局部变量并赋值或者定义局部函数 */
      luaX_next(ls);  /* skip LOCAL 识别下一个符号 丢弃识别的local */
      if (testnext(ls, TK_FUNCTION))  /* local function? 定义局部函数 */
        localfunc(ls);
      else  // 现在一定是定义局部变量
        localstat(ls);
      break;
    }
    case TK_DBCOLON: {  /* stat -> label 识别到双冒号是标签声明语句 */
      luaX_next(ls);  /* skip double colon */
      labelstat(ls, str_checkname(ls), line);
      break;
    }
    // 识别return语句
    case TK_RETURN: {  /* stat -> retstat */
      luaX_next(ls);  /* skip RETURN */
      retstat(ls);
      break;
    }
    case TK_BREAK:   /* stat -> breakstat */
    case TK_GOTO: {  /* stat -> 'goto' NAME */
      gotostat(ls, luaK_jump(ls->fs));
      break;
    }
    default: {  /* stat -> func | assignment 函数调用或者赋值语句 */
      exprstat(ls);
      break;
    }
  }
  lua_assert(ls->fs->f->maxstacksize >= ls->fs->freereg &&
             ls->fs->freereg >= ls->fs->nactvar);
  // 使用的寄存器个数就是定义的局部变量个数 其他的都去掉
  // 每次识别完一条语句后就清空所有的临时变量
  ls->fs->freereg = ls->fs->nactvar;  /* free registers */
  leavelevel(ls);
}

/* }====================================================================== */


/*
** compiles the main function, which is a regular vararg function with an
** upvalue named LUA_ENV
*/
// 编译主函数
// 主函数是一个普通的可变参数函数 有一个upvalue为_ENV
static void mainfunc (LexState *ls, FuncState *fs) {
  BlockCnt bl;
  expdesc v;
  // luaY_parser中新建了FuncState和Propo对象,这里只新定义了BlockCnt就可以调用open_func
  open_func(ls, fs, &bl);
  // 主函数一定是可变参数函数 最外层使用...接收的是命令行中传递的参数
  fs->f->is_vararg = 1;  /* main function is always declared vararg */
  // 初始化_ENV的表达式信息,假定它是主函数外面函数0位置的局部变量
  init_exp(&v, VLOCAL, 0);  /* create and... */
  // 根据表达式信息和变量名创建环境变量 _ENV
  newupvalue(fs, ls->envn, &v);  /* ...set environment upvalue */
  // 读入第一个符号
  luaX_next(ls);  /* read first token */
  // 识别整个主函数
  statlist(ls);  /* parse main body */
  // 调用statlist之后应该整个文件都结束了
  // 如果return语句后还有语句那么就会报错
  check(ls, TK_EOS);
  close_func(ls);    // 关闭函数
}


// 语法分析的启动函数
// 语法分析的过程是生成一个Proto对象树的过程
// 每个Proto对象包含了它的子函数(p),局部变量及其作用域(locvars),upvalue(upvalues),所有指令(code),所有常量(k)
// FuncState和BlockCnt构成栈结构
// firstchar是读取文件的第一个字符,f_parser函数中为了区分文本文件或者二进制文件已经读取了第一个字符
// 在栈顶生成了主函数闭包并返回,调用mainfunc来生成指令
// 执行过程中又往栈顶压入了一个表,用来缓存语法分析过程中用到的字符串,结束后top--丢弃该表
LClosure *luaY_parser (lua_State *L, ZIO *z, Mbuffer *buff,
                       Dyndata *dyd, const char *name, int firstchar) {
  LexState lexstate;   // 初始化词法状态
  FuncState funcstate; // 初始化函数状态
  // 编译期间生成Proto对象代表函数原型, closure对象可以算作函数的实例
  // 主函数有1个upvalue也就是_ENV所以这里传1
  LClosure *cl = luaF_newLclosure(L, 1);  /* create main closure 主函数闭包 */
  // 将主函数闭包压入栈顶 栈顶自增, 放入栈顶等待之后的调用
  setclLvalue(L, L->top, cl);  /* anchor it (to avoid being collected) */
  luaD_inctop(L);
  // 创建空表 用于缓存常量值
  lexstate.h = luaH_new(L);  /* create table for scanner */
  // 将该空表压入栈顶 栈顶自增
  sethvalue(L, L->top, lexstate.h);  /* anchor it */
  luaD_inctop(L);
  // 初始化Propo对象
  // 只有主函数的Proto对象通过luaF_newproto函数直接创建
  // 其他函数均在body函数中调用addprototype函数创建
  // 因为主函数不是任何函数的子函数,不用加入到f->p数组内
  funcstate.f = cl->p = luaF_newproto(L);
  // source保存文件名(@xxx) xxx是文件名
  funcstate.f->source = luaS_new(L, name);  /* create and anchor TString */
  lua_assert(iswhite(funcstate.f));  /* do not need barrier here */
  lexstate.buff = buff;
  lexstate.dyd = dyd;
  // 初始化dyd中三个列表的大小为0
  dyd->actvar.n = dyd->gt.n = dyd->label.n = 0;
  // 为词法分析设置输入文件
  luaX_setinput(L, &lexstate, z, funcstate.f->source, firstchar);
  // 此时lexstate的t,fs还未初始化
  // funcstat的prev为NULL ls,bl未初始化
  mainfunc(&lexstate, &funcstate);
  // 主函数的prev一定为NULL 有一个upvalue为_ENV
  // 最终lexstate.fs应该为NULL,调用mainfunc中有close_func使lexstate.fs指向lexstate.fs->prev也就是NULL
  lua_assert(!funcstate.prev && funcstate.nups == 1 && !lexstate.fs);
  /* all scopes should be correctly finished */
  lua_assert(dyd->actvar.n == 0 && dyd->gt.n == 0 && dyd->label.n == 0);
  // 去掉词法分析器使用的表
  L->top--;  /* remove scanner's table */
  // 现在栈顶是主函数闭包
  return cl;  /* closure is on the stack, too */
}
