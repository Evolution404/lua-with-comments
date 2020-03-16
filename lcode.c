/*
** $Id: lcode.c,v 2.112 2016/12/22 13:08:50 roberto Exp $
** Code generator for Lua Lua代码生成器
** See Copyright Notice in lua.h
*/

#define lcode_c
#define LUA_CORE

#include "lprefix.h"


#include <math.h>
#include <stdlib.h>

#include "lua.h"

#include "lcode.h"
#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lmem.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lparser.h"
#include "lstring.h"
#include "ltable.h"
#include "lvm.h"


/* Maximum number of registers in a Lua function (must fit in 8 bits) */
// 一个函数寄存器个数的最大值
#define MAXREGS		255


// t和f不相等说明有回填链
#define hasjumps(e)	((e)->t != (e)->f)


/*
** If expression is a numeric constant, fills 'v' with its value
** and returns 1. Otherwise, returns 0.
*/
// 判断表达式e是不是一个单纯的数字常量
// 返回1 说明e是一个数字常量 并在v不是NULL的情况下将数字写入v
// 返回0 说明e不是一个数字常量
static int tonumeral(const expdesc *e, TValue *v) {
  // 有跳转链不能单纯的判断是否是数字
  if (hasjumps(e))
    return 0;  /* not a numeral */
  switch (e->k) {
    case VKINT:
      if (v) setivalue(v, e->u.ival);
      return 1;
    case VKFLT:
      if (v) setfltvalue(v, e->u.nval);
      return 1;
    default: return 0;
  }
}


/*
** Create a OP_LOADNIL instruction, but try to optimize: if the previous
** instruction is also OP_LOADNIL and ranges are compatible, adjust
** range of previous instruction instead of emitting a new one. (For
** instance, 'local a; local b' will generate a single opcode.)
*/
// 生成OP_LOADNIL指令 from是开始的位置 n是设置nil的个数
// 可能生成新指令,也可能直接修改当前最后一条OP_LOADNIL指令的开始结束范围
// 生成过程如果前一个指令也是OP_LOADNIL而且可以与当前要生成的指令合并就将两者合并
void luaK_nil (FuncState *fs, int from, int n) {
  Instruction *previous;
  int l = from + n - 1;  /* last register to set nil l是最后一个设置nil的位置 */
  // 合并成功之后是直接修改原来的指令并不会生成新指令
  // 如果有跳转到当前位置的指令那么位置就不对了 所以有跳转到当前位置的指令就不尝试合并
  if (fs->pc > fs->lasttarget) {  /* no jumps to current position? 没有要跳转到pc的指令 */
    // 如果前一个指令是OP_LOADNIL指令将它与要生成的指令合并
    previous = &fs->f->code[fs->pc-1];
    if (GET_OPCODE(*previous) == OP_LOADNIL) {  /* previous is LOADNIL? 是OP_LOADNIL指令 */
      // 计算出来上一条指令加载nil的开始结束位置
      int pfrom = GETARG_A(*previous);  /* get previous range */
      int pl = pfrom + GETARG_B(*previous);
      // from在pfrom和pl之间 或者 pfrom在from和l之间说明两个指令可以合并
      if ((pfrom <= from && from <= pl + 1) ||
          (from <= pfrom && pfrom <= l + 1)) {  /* can connect both? */
        // from取pfrom和from较小者
        if (pfrom < from) from = pfrom;  /* from = min(from, pfrom) */
        // l取pl和l较大者
        if (pl > l) l = pl;  /* l = max(l, pl) */
        // 直接修改上一条指令
        SETARG_A(*previous, from);
        SETARG_B(*previous, l - from);
        // 修改完毕直接返回不需要生成新的指令
        return;
      }
    }  /* else go through */
  }
  // OP_LOADNIL的B参数代表加载nil的个数-1
  luaK_codeABC(fs, OP_LOADNIL, from, n - 1, 0);  /* else no optimization */
}


/*
** Gets the destination address of a jump instruction. Used to traverse
** a list of jumps.
*/
// 传入当前指令位置 计算跳转到指令的位置
// 传入的值和返回的值都是指令的绝对位置,OP_JMP指令存的是相对位置
// 返回NO_JUMP说明结束
static int getjump (FuncState *fs, int pc) {
  // offset=-1说明指向自己 0为下一个 -2为前一个
  int offset = GETARG_sBx(fs->f->code[pc]);
  if (offset == NO_JUMP)  /* point to itself represents end of list */
    return NO_JUMP;  /* end of list */
  else
    return (pc+1)+offset;  /* turn offset into absolute position 计算出跳转指令的位置 */
}


/*
** Fix jump instruction at position 'pc' to jump to 'dest'.
** (Jump addresses are relative in Lua)
*/
// 设置跳转指令pc跳转到dest指令
// 计算出pc和dest的偏移量 设置pc位置指令的sBx参数为偏移量
static void fixjump (FuncState *fs, int pc, int dest) {
  Instruction *jmp = &fs->f->code[pc];
  int offset = dest - (pc + 1);
  lua_assert(dest != NO_JUMP);
  if (abs(offset) > MAXARG_sBx)
    luaX_syntaxerror(fs->ls, "control structure too long");
  SETARG_sBx(*jmp, offset);
}


/*
** Concatenate jump-list 'l2' into jump-list 'l1'
*/
// 在跳转链的末尾加上l2位置的指令
// l2是-1 直接返回
// *l1如果是-1 那么就让l2当跳转链的头
void luaK_concat (FuncState *fs, int *l1, int l2) {
  if (l2 == NO_JUMP) return;  /* nothing to concatenate? */
  else if (*l1 == NO_JUMP)  /* no original list? */
    *l1 = l2;  /* 'l1' points to 'l2' */
  else {
    int list = *l1;
    int next;
    // 遍历跳转链找到最后一个指令(跳转位置是-1)
    while ((next = getjump(fs, list)) != NO_JUMP)  /* find last element */
      list = next;
    // 现在list位置指令的跳转位置是-1 设置list位置指令的下一跳为l2
    fixjump(fs, list, l2);  /* last element links to 'l2' */
  }
}


/*
** Create a jump instruction and return its position, so its destination
** can be fixed later (with 'fixjump'). If there are jumps to
** this position (kept in 'jpc'), link them all together so that
** 'patchlistaux' will fix all them directly to the final destination.
*/
// 生成一个跳转指令 返回新生成的跳转指令
// 生成跳转指令时如果jpc不为NO_JUMP说明有指令跳转到这个新增跳转指令
// 如果将新增的跳转指令回填到jpc中那么jpc链的这些指令都要跳转到新增指令,在跳转到新增指令要跳转的指令
// 这里将新增指令插入到jpc链的头 将来回填时jpc链和新增跳转指令都跳转到新增指令的跳转位置
// 这样jpc链中的指令由两次跳转变成了一次跳转 进行了一次优化
int luaK_jump (FuncState *fs) {
  // 保存当前的回填链
  int jpc = fs->jpc;  /* save list of jumps to here */
  int j;
  // 清空回填链 不应该有跳转到跳转指令的跳转指令
  fs->jpc = NO_JUMP;  /* no more jumps to here */
  // 生成跳转指令
  j = luaK_codeAsBx(fs, OP_JMP, 0, NO_JUMP);
  // 将新生成的跳转指令插到原来的回填链头
  luaK_concat(fs, &j, jpc);  /* keep them on hold */
  // 返回新生成的跳转指令或者说一条回填链
  return j;
}


/*
** Code a 'return' instruction
*/
// 生成OP_RETURN指令
void luaK_ret (FuncState *fs, int first, int nret) {
  // 参数B是返回值个数+1
  luaK_codeABC(fs, OP_RETURN, first, nret+1, 0);
}


/*
** Code a "conditional jump", that is, a test or comparison opcode
** followed by a jump. Return jump position.
*/
// 先生成输入的指令(测试指令或比较指令)
// 后面再生成一个跳转指令 最后返回跳转指令的位置
static int condjump (FuncState *fs, OpCode op, int A, int B, int C) {
  luaK_codeABC(fs, op, A, B, C);
  return luaK_jump(fs);
}


/*
** returns current 'pc' and marks it as a jump target (to avoid wrong
** optimizations with consecutive instructions not in the same basic block).
*/
// 修改lasttarget为pc并返回当前pc
int luaK_getlabel (FuncState *fs) {
  fs->lasttarget = fs->pc;
  return fs->pc;
}


/*
** Returns the position of the instruction "controlling" a given
** jump (that is, its condition), or the jump itself if it is
** unconditional.
*/
// 跳转指令有可能只是单纯的跳转指令或者是被测试指令控制
// 每一条测试指令的下一条指令一定都是跳转指令
// 如果pc的跳转指令由测试指令控制返回控制pc的测试指令
// 如果pc的跳转指令只是单纯的跳转指令返回它自己
static Instruction *getjumpcontrol (FuncState *fs, int pc) {
  Instruction *pi = &fs->f->code[pc];
  // pc>=1才能有前一条指令且前一条指令是测试指令
  if (pc >= 1 && testTMode(GET_OPCODE(*(pi-1))))
    return pi-1;  // 当前跳转指令由它前一条测试指令控制
  else
    return pi;
}


/*
** Patch destination register for a TESTSET instruction.
** If instruction in position 'node' is not a TESTSET, return 0 ("fails").
** Otherwise, if 'reg' is not 'NO_REG', set it as the destination
** register. Otherwise, change instruction to a simple 'TEST' (produces
** no register value)
*/
// 跳转指令node紧跟在OP_TESTSET指令后面返回1 否则返回0
// OP_TESTSET指令A参数需要被回填
// 如果node紧跟在OP_TESTSET指令后
//   如果能回填   会对OP_TESTSET指令的参数A进行回填
//   如果不能回填 转换该OP_TESTSET指令为OP_TEST指令
static int patchtestreg (FuncState *fs, int node, int reg) {
  Instruction *i = getjumpcontrol(fs, node);  // 获得node的控制指令
  if (GET_OPCODE(*i) != OP_TESTSET)  // 不是OP_TESTSET直接返回0
    return 0;  /* cannot patch other instructions */
  // 必须有要回填的寄存器位置 参数B不能与参数A相等
  // OP_TESTSET的含义是 (R(B)==C) then R(A)=R(B) else pc++
  // 如果reg与参数B相等,无论R(B)==C的结果如何寄存器结果都不变,相等的话重复赋值一次
  // 这样就等价于OP_TEST指令,可以进行优化
  if (reg != NO_REG && reg != GETARG_B(*i))
    SETARG_A(*i, reg);  // 回填OP_TESTSET指令的A参数
  else {
     /* no register to put value or register already has the value;
        change instruction to simple test */
    // 没有寄存器去放结果或者寄存器上已经有值了 修改OP_TESTSET指令为OP_TEST指令
    *i = CREATE_ABC(OP_TEST, GETARG_B(*i), 0, GETARG_C(*i));
  }
  return 1;
}


/*
** Traverse a list of tests ensuring no one produces a value
*/
// 遍历list回填链 如果跳转指令的控制指令有OP_TESTSET指令,那么就修改为OP_TEST
// 确保list回填链中不会有指令会产生一个值
static void removevalues (FuncState *fs, int list) {
  for (; list != NO_JUMP; list = getjump(fs, list))
      patchtestreg(fs, list, NO_REG);
}


/*
** Traverse a list of tests, patching their destination address and
** registers: tests producing values jump to 'vtarget' (and put their
** values in 'reg'), other tests jump to 'dtarget'.
*/
// 遍历回填链 填充目标地址
// vtarget: value target   是表达式的结束
// dtarget: default target 是OP_LOADBOOL指令
// 回填链中各个跳转指令可能使用vtarget回填也可能使用dtarget回填
// 控制指令是OP_TESTSET使用vtartget回填
// 控制指令不是OP_TESTSET使用dtartget回填 因为需要跳转OP_LOADBOOL指令
static void patchlistaux (FuncState *fs, int list, int vtarget, int reg,
                          int dtarget) {
  while (list != NO_JUMP) {  // 遍历回填链
    int next = getjump(fs, list);
    if (patchtestreg(fs, list, reg))  // 区分是否紧跟在OP_TESTSET指令后面
      fixjump(fs, list, vtarget);     // OP_TESTSET后面的跳转指令直接回填
    else
      // 不是OP_TESTSET后面的跳转指令有可能跳转到OP_LOADBOOL指令上
      fixjump(fs, list, dtarget);  /* jump to default target */
    list = next;
  }
}


/*
** Ensure all pending jumps to current position are fixed (jumping
** to current position with no values) and reset list of pending
** jumps
*/
// 用pc回填jpc链 回填完毕重置jpc为NO_JUMP
static void dischargejpc (FuncState *fs) {
  patchlistaux(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
  fs->jpc = NO_JUMP;
}


/*
** Add elements in 'list' to list of pending jumps to "here"
** (current position)
*/
// list是需要以pc回填的回填链
// 将list链串到jpc链后面 以便dischargejpc函数回填jpc
// 修改lasttarget为pc
void luaK_patchtohere (FuncState *fs, int list) {
  // 标记pc为上一次跳转目标
  luaK_getlabel(fs);  /* mark "here" as a jump target */
  luaK_concat(fs, &fs->jpc, list);
}


/*
** Path all jumps in 'list' to jump to 'target'.
** (The assert means that we cannot fix a jump to a forward address
** because we only know addresses once code is generated.)
*/
// 如果target是fs->pc那么就将list串到fs->jpc后面
// 否则使用target回填list回填链
void luaK_patchlist (FuncState *fs, int list, int target) {
  // 如果目标位置就是pc那么就将list加到jpc链上 等到dischargejpc来回填
  if (target == fs->pc)  /* 'target' is current position? */
    luaK_patchtohere(fs, list);  /* add list to pending jumps */
  // target不是pc立即回填
  else {
    // target一定小于pc 还没生成的指令不可能知道要跳转到还没生成的指令
    lua_assert(target < fs->pc);
    patchlistaux(fs, list, target, NO_REG, target);
  }
}


/*
** Path all jumps in 'list' to close upvalues up to given 'level'
** (The assertion checks that jumps either were closing nothing
** or were closing higher levels, from inner blocks.)
*/
// 该函数用level+1来回填list链中OP_JMP指令的参数A
// level代表要关闭upvalues>=R(level)
// list是要回填的回填链
// 使用level+1回填list回填链的OP_JMP指令中的A参数
void luaK_patchclose (FuncState *fs, int list, int level) {
  // OP_JMP关闭所有upvalues>=R(A-1)
  // 如果A=0不做操作 A=1就关闭0和0以上的upvalue
  // 这里level本来代表的是关闭upvalues的位置 加1转换成OP_JMP指令A参数的格式
  level++;  /* argument is +1 to reserve 0 as non-op */
  for (; list != NO_JUMP; list = getjump(fs, list)) {
    // 确保指令是跳转指令 要么不关闭upvalue 要么关闭的upvalue>=level
    // 不可能存在原来要关闭的upvalue比现在还多
    lua_assert(GET_OPCODE(fs->f->code[list]) == OP_JMP &&
                (GETARG_A(fs->f->code[list]) == 0 ||
                 GETARG_A(fs->f->code[list]) >= level));
    // 设置指令的A参数
    SETARG_A(fs->f->code[list], level);
  }
}


/*
** Emit instruction 'i', checking for array sizes and saving also its
** line information. Return 'i' position.
*/
// 在fs->f->code中新增指令i 返回指令i的下标
// 新增指令前要遍历jpc 使用pc回填jpc回填链
static int luaK_code (FuncState *fs, Instruction i) {
  Proto *f = fs->f;
  dischargejpc(fs);  /* 'pc' will change */
  /* put new instruction in code array */
  // 确保空间足够
  luaM_growvector(fs->ls->L, f->code, fs->pc, f->sizecode, Instruction,
                  MAX_INT, "opcodes");
  // 将新的指令i放入code数组中
  f->code[fs->pc] = i;
  /* save corresponding line information */
  // 将行信息写入lineinfo数组中
  luaM_growvector(fs->ls->L, f->lineinfo, fs->pc, f->sizelineinfo, int,
                  MAX_INT, "opcodes");
  f->lineinfo[fs->pc] = fs->ls->lastline;  // lineinfo[k]代表code[k]所在的行
  return fs->pc++;  // 返回新增指令的下标
}


/*
** Format and emit an 'iABC' instruction. (Assertions check consistency
** of parameters versus opcode.)
*/
// 生成iABC类型的指令
int luaK_codeABC (FuncState *fs, OpCode o, int a, int b, int c) {
  // 检查参数与指令码的一致性
  lua_assert(getOpMode(o) == iABC);  // 确保是iABC指令
  // 保证不存在B参数不为0且B参数不被使用的情况
  lua_assert(getBMode(o) != OpArgN || b == 0);
  // 保证不存在C参数不为0且C参数不被使用的情况
  lua_assert(getCMode(o) != OpArgN || c == 0);
  // ABC都在合适的范围
  lua_assert(a <= MAXARG_A && b <= MAXARG_B && c <= MAXARG_C);
  // 实际生成指令的代码
  return luaK_code(fs, CREATE_ABC(o, a, b, c));
}


/*
** Format and emit an 'iABx' instruction.
*/
// 生成iABx类型的指令 生成iAsBx类型指令也是利用这个函数
// bc参数是Bx或者sBx 一定是大于等于0的正数 对于sBx负数用0-MAXARG_sBx表示
int luaK_codeABx (FuncState *fs, OpCode o, int a, unsigned int bc) {
  // 指令类型可以是iABx或者iAsBx
  lua_assert(getOpMode(o) == iABx || getOpMode(o) == iAsBx);
  // 参数C不被使用
  lua_assert(getCMode(o) == OpArgN);
  // 参数Å和参数B在正确的范围
  lua_assert(a <= MAXARG_A && bc <= MAXARG_Bx);
  // 构建指令
  return luaK_code(fs, CREATE_ABx(o, a, bc));
}


/*
** Emit an "extra argument" instruction (format 'iAx')
*/
// 生成OP_EXTRAARG指令类型是iAx指令 a是指令中的A参数
static int codeextraarg (FuncState *fs, int a) {
  lua_assert(a <= MAXARG_Ax);
  return luaK_code(fs, CREATE_Ax(OP_EXTRAARG, a));
}


/*
** Emit a "load constant" instruction, using either 'OP_LOADK'
** (if constant index 'k' fits in 18 bits) or an 'OP_LOADKX'
** instruction with "extra argument".
*/
// 生成加载常量的指令 OP_LOADK或OP_LOADKX 返回生成指令的下标
// reg是放置常量的寄存器位置 k是常量在常量表的位置
// OP_LOADKX需要一条额外的OP_EXTRAARG指令
int luaK_codek (FuncState *fs, int reg, int k) {
  if (k <= MAXARG_Bx)  // k能够在OP_LOADK指令中放下
    return luaK_codeABx(fs, OP_LOADK, reg, k);
  else {  // 使用OP_LOADKX指令
    int p = luaK_codeABx(fs, OP_LOADKX, reg, 0); // 生成OP_LOADKX指令
    codeextraarg(fs, k);                         // 生成OP_EXTRAARG指令
    return p;
  }
}


/*
** Check register-stack level, keeping track of its maximum size
** in field 'maxstacksize'
*/
// 该函数用来记录一个函数原型最多占用多少栈空间,也就是maxstacksize
// 如果栈上还有n个空间直接结束
// 如果栈上空间不足n个
//   如果新增n个空间后超过上限 报错
//   否则让maxstacksize设置为原来占用空间加上n
void luaK_checkstack (FuncState *fs, int n) {
  int newstack = fs->freereg + n;
  if (newstack > fs->f->maxstacksize) {  // 新空间大于maxstacksize
    if (newstack >= MAXREGS)  // 超过上限了报错
      luaX_syntaxerror(fs->ls,
        "function or expression needs too many registers");
    fs->f->maxstacksize = cast_byte(newstack);  // 修改maxstacksize为新空间大小
  }
}


/*
** Reserve 'n' registers in register stack
*/
// 首先确保有n个空间 让freereg后移n位
void luaK_reserveregs (FuncState *fs, int n) {
  luaK_checkstack(fs, n);  // 确保有n个空间
  fs->freereg += n;
}


/*
** Free register 'reg', if it is neither a constant index nor
** a local variable.
)
*/
// 确保reg位置不是常量且不是局部变量 让freereg前移一位
static void freereg (FuncState *fs, int reg) {
  // 不能释放常量或者局部变量,释放的是临时存在寄存器里的值
  if (!ISK(reg) && reg >= fs->nactvar) {
    fs->freereg--;
    // 被释放的位置应该就是当前最后一个寄存器
    lua_assert(reg == fs->freereg);
  }
}


/*
** Free register used by expression 'e' (if any)
*/
// 释放表达式e使用的寄存器空间
// 类型是VNONRELOC且占用位置在局部变量之后,所以释放的是临时存在寄存器里的值
static void freeexp (FuncState *fs, expdesc *e) {
  if (e->k == VNONRELOC)    // 表达式类型是固定寄存器位置
    freereg(fs, e->u.info); // 释放表达式的空间
}


/*
** Free registers used by expressions 'e1' and 'e2' (if any) in proper
** order.
*/
// 按照适当的顺序释放e1和e2两个表达式使用的寄存器的空间
// 不是VNONRELOC类型的表达式这个函数什么也不做
static void freeexps (FuncState *fs, expdesc *e1, expdesc *e2) {
  // 固定寄存器位置的表达式才能释放
  int r1 = (e1->k == VNONRELOC) ? e1->u.info : -1;
  int r2 = (e2->k == VNONRELOC) ? e2->u.info : -1;
  // 调用freereg输入的reg一定是freereg前一个寄存器
  // r1和r2要取位置较大的先调用freereg
  // freereg传入-1不做任何操作
  if (r1 > r2) {
    freereg(fs, r1);
    freereg(fs, r2);
  }
  else {
    freereg(fs, r2);
    freereg(fs, r1);
  }
}


/*
** Add constant 'v' to prototype's list of constants (field 'k').
** Use scanner's table to cache position of constants in constant list
** and try to reuse constants. Because some values should not be used
** as keys (nil cannot be a key, integer keys can collapse with float
** keys), the caller must provide a useful 'key' for indexing the cache.
*/
// 将v添加到常量表中 常量表是fs->f->k
// fs->ls->h表用来缓存 key是要缓存的值,value保存了该值在常量表中的下标
// 先查询fs->ls->h[key] 如果有值它是常量表下标,继续使用该下标查询常量表的值比较与v是否一样
// 如果没有值 在fs->ls->h表中缓存 在fs->f->k的常量表中增加v
// 返回值是v在常量表的位置
// 这个函数并不能保证lua中每个函数的常量表中没有重复的值,例如
// aa = function() print("aa") end
// aa = "aa"
// 首先在主函数中创建了aa常量
// 然后进入子函数查询aa找到下标但是比较常量表值不相同所以在子函数中重新创建了aa
// 这个过程覆盖了主函数aa的下标,回到主函数又识别到aa,现在缓存表中存的是子函数的下标
// 所以比较后值不相同又在常量表中新建了一项
static int addk (FuncState *fs, TValue *key, TValue *v) {
  lua_State *L = fs->ls->L;
  Proto *f = fs->f;
  // 在ls->h表中创建key项或者查询key项
  TValue *idx = luaH_set(L, fs->ls->h, key);  /* index scanner table */
  int k, oldsize;
  if (ttisinteger(idx)) {  /* is there an index there? 查询到的值是整数 */
    k = cast_int(ivalue(idx));  // k就是f->k的下标
    /* correct value? (warning: must distinguish floats from integers!) */
    // 首先检查下标k是有效的
    // 再确保常量数组中k位置的值与传进来的v一样
    // 因为有可能已经缓存在了h表中,但是存在了别的函数的常量表中
    // 这时候虽然命中缓存,但是依然要在该函数的常量表增加一项存入v
    if (k < fs->nk && ttype(&f->k[k]) == ttype(v) &&
                      luaV_rawequalobj(&f->k[k], v))
      return k;  /* reuse index */
  }
  /* constant not found; create a new entry */
  oldsize = f->sizek;
  k = fs->nk;
  /* numerical value does not need GC barrier;
     table has no metatable, so it does not need to invalidate cache */
  // ls->h表的value部分存储了key在f->k的下标
  setivalue(idx, k);
  // 扩张Proto对象的常量表
  luaM_growvector(L, f->k, k, f->sizek, TValue, MAXARG_Ax, "constants");
  // 初始化常量表增加的空间
  while (oldsize < f->sizek) setnilvalue(&f->k[oldsize++]);
  // 向常量表中添加v
  setobj(L, &f->k[k], v);
  fs->nk++;
  luaC_barrier(L, f, v);
  return k;
}


/*
** Add a string to list of constants and return its index.
*/
// 将字符串加入常量表 返回该字符串在常量表中的下标
int luaK_stringK (FuncState *fs, TString *s) {
  TValue o;
  setsvalue(fs->ls->L, &o, s);
  return addk(fs, &o, &o);  /* use string itself as key */
}


/*
** Add an integer to list of constants and return its index.
** Integers use userdata as keys to avoid collision with floats with
** same value; conversion to 'void*' is used only for hashing, so there
** are no "precision" problems.
*/
// 将整数加入常量表 返回该整数在常量表中的下标
// 使用userdata作为key是为了避免与相同值的浮点数冲突
// 类型转换成'void*'只是用于求哈希不会导致精度损失
int luaK_intK (FuncState *fs, lua_Integer n) {
  TValue k, o;
  setpvalue(&k, cast(void*, cast(size_t, n)));
  setivalue(&o, n);
  return addk(fs, &k, &o);
}

/*
** Add a float to list of constants and return its index.
*/
// 将浮点数加入常量表 返回该浮点数在常量表中的下标
static int luaK_numberK (FuncState *fs, lua_Number r) {
  TValue o;
  setfltvalue(&o, r);
  return addk(fs, &o, &o);  /* use number itself as key */
}


/*
** Add a boolean to list of constants and return its index.
*/
// 将布尔值加入常量表 返回该布尔值在常量表中的下标
static int boolK (FuncState *fs, int b) {
  TValue o;
  setbvalue(&o, b);
  return addk(fs, &o, &o);  /* use boolean itself as key */
}


/*
** Add nil to list of constants and return its index.
*/
// 将nil加入常量表 返回nil在常量表中的下标
static int nilK (FuncState *fs) {
  TValue k, v;
  setnilvalue(&v);
  /* cannot use nil as key; instead use table itself to represent nil */
  // nil不能作为key 这里用fs->ls->h表自己作为key代表nil值
  sethvalue(fs->ls->L, &k, fs->ls->h);
  return addk(fs, &k, &v);
}


/*
** Fix an expression to return the number of results 'nresults'.
** Either 'e' is a multi-ret expression (function call or vararg)
** or 'nresults' is LUA_MULTRET (as any expression can satisfy that).
*/
// 该函数用于设置表达式e代表的函数调用或者...变量的返回值个数
// e是函数调用或可变参数 其他情况nresults一定是LUA_MULTRET
// nresults是指返回值的个数 如果是-1说明是返回多个值 0不返回 1或更大就是返回的个数
// 对于e是VCALL类型 回填OP_CALL指令的C参数
// 对于e是VVARARG类型 回填OP_VARARG指令的A和B参数
void luaK_setreturns (FuncState *fs, expdesc *e, int nresults) {
  if (e->k == VCALL) {  /* expression is an open function call? 表达式是一个函数调用 */
    // OP_CALL指令的参数C和nresults相差1
    SETARG_C(getinstruction(fs, e), nresults + 1);  // 参数C是返回值个数+1
  }
  else if (e->k == VVARARG) {  // 是可变参数
    Instruction *pc = &getinstruction(fs, e);
    // 从当前freereg位置开始加载nresults个值到寄存器
    SETARG_B(*pc, nresults + 1);  // 参数B是返回值个数+1
    SETARG_A(*pc, fs->freereg);   // 参数A是使用的寄存器开始位置
    // VCALL类型本身调用的函数占用了一个寄存器
    // 所以为了保存返回值值只需要再分配nresults-1个寄存器
    // 这里分配一个寄存器可能是为了与VCALL类型统一
    // 所有多返回值类型设置返回值后还需要再分配nresults个寄存器
    luaK_reserveregs(fs, 1);
  }
  else lua_assert(nresults == LUA_MULTRET);
}


/*
** Fix an expression to return one result.
** If expression is not a multi-ret expression (function call or
** vararg), it already returns one result, so nothing needs to be done.
** Function calls become VNONRELOC expressions (as its result comes
** fixed in the base register of the call), while vararg expressions
** become VRELOCABLE (as OP_VARARG puts its results where it wants).
** (Calls are created returning one result, so that does not need
** to be fixed.)
*/
// e如果是VCALL修改成VNONRELOC 修改info为指令的参数A
// e如果是VVARARG修改成VRELOCABLE 设置指令的参数B是2
// VCALL的返回值位置是固定的,就是用来存放被调用closure的寄存器,所以被转化为VNONRELOC类型
// VVARARG的目标寄存器待定,被转化为VRELOCABLE类型
void luaK_setoneret (FuncState *fs, expdesc *e) {
  // 是函数调用
  if (e->k == VCALL) {  /* expression is an open function call? */
    /* already returns 1 value */
    // 已经返回了一个值 参数C一定是2
    lua_assert(GETARG_C(getinstruction(fs, e)) == 2);
    e->k = VNONRELOC;  /* result has fixed position */
    // 返回的结果就保存在被调用的函数上
    e->u.info = GETARG_A(getinstruction(fs, e));
  }
  else if (e->k == VVARARG) {  // 是可变参数
    // 因为返回一个值所以参数B一定是2 参数A现在还不知道需要回填 所以类型设置为VRELOCABLE
    SETARG_B(getinstruction(fs, e), 2);
    e->k = VRELOCABLE;  /* can relocate its simple result */
  }
}


/*
** Ensure that expression 'e' is not a variable.
*/
// 执行后表达式类型可能为VNIL,VTRUE,VFALSE,VK,VKFLT,VKINT,VNONRELOC,VRELOCABLE
// 确保表达式e不可能是VLOCAL,VUPVAL,VINDEXED,VVARARG,VCALL
// 根据变量所在的不同作用域(local，global，upvalue)等来决定这个变量是否需要重定向
// VLOCAL
//   e->k修改为VNONRELOC
//   e->u.info没有修改 存放局部变量的位置
// VUPVAL
//   e->k修改为VRELOCABLE
//   新生成OP_GETUPVAL指令 指令参数A需要回填 参数B为原info
//   e->u.info修改为新指令的位置
// VINDEXED
//   e->k修改为VRELOCABLE
//   新生成OP_GETTABLE或OP_GETTABUP指令 指令参数A需要回填 参数B为表位置 参数C为key位置
//   e->u.info修改为新指令的位置
// VVARARG
//   e->k修改为VRELOCABLE
//   e->u.info没有修改
//   原info存放指令修改B参数为2,需要回填参数A
// VCALL
//   e->k修改为VNONRELOC
//   e->u.info修改为原info代表指令的参数A
//   原info存放指令不变
// 其他类型不做操作
void luaK_dischargevars (FuncState *fs, expdesc *e) {
  switch (e->k) {
    // 局部变量直接读就行不需要从外部拿进来所以就是VNONRELOC
    case VLOCAL: {  /* already in a register */
      e->k = VNONRELOC;  /* becomes a non-relocatable value */
      break;
    }
    // upvalue是从外部查找到的需要先移动到内部来
    // 但是现在不知道要移动内部的哪个位置 所以OP_GETUPVAL的参数A现在悬空为0
    // 而且要设置类型为VRELOCABLE 说明这个表达式重定向了需要回填
    case VUPVAL: {  /* move value to some (pending) register */
      // info存着upvalue的位置 修改info为新建OP_GETUPVAL指令的位置
      // 参数A是0,因为还不知道要加载到什么位置
      // 参数B是原来的info,因为原来的info存放着upvalue的位置
      e->u.info = luaK_codeABC(fs, OP_GETUPVAL, 0, e->u.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    // ind.t代表被查询的表的位置 ind.idx代表查询的key的位置,可能是常量位置或者寄存器位置
    case VINDEXED: {
      OpCode op;
      freereg(fs, e->u.ind.idx);  // 查询后key就不会用到了,释放掉
      // 区分表t的是upvalue还是局部变量,使用不同的指令
      if (e->u.ind.vt == VLOCAL) {  /* is 't' in a register? 表t是局部变量 */
        // 查询后表本身也不会用到了
        freereg(fs, e->u.ind.t);
        op = OP_GETTABLE;
      }
      else {  // 表t不是局部变量 一定是upvalue
        // 表t一定是upvalue不应该调用freereg
        lua_assert(e->u.ind.vt == VUPVAL);
        op = OP_GETTABUP;  /* 't' is in an upvalue */
      }
      // 一定是重定向了 现在不知道查询表的结果要放到哪
      e->u.info = luaK_codeABC(fs, op, 0, e->u.ind.t, e->u.ind.idx);
      e->k = VRELOCABLE;
      break;
    }
    // VVARARG设置为VRELOCABLE 需要设置info和指令的A参数
    // VCALL设置为VNONRELOC    指令不需要回填,info已经指向了值所在寄存器
    case VVARARG: case VCALL: {
      luaK_setoneret(fs, e);
      break;
    }
    default: break;  /* there is one value available (somewhere) */
  }
}


/*
** Ensures expression value is in register 'reg' (and therefore
** 'e' will become a non-relocatable expression).
*/
// 调用后表达式类型为VNONRELOC或者VJMP
// 除VJMP类型外,将所有类型表达式存入寄存器,类型修改VNONRELOC,info指向寄存器位置
// VNIL VFALSE VTRUE VK VKFLT VKINT
//   生成一条加载各自类型到reg寄存器的指令 VKxx需要将值先保存的常量表中
// VLOCAL
//   如果e->u.info与reg不同生成一条OP_MOVE指令移动值到reg寄存器
// VUPVAL
//   新生成OP_GETUPVAL指令 指令参数A为reg 参数B为原info
// VINDEXED
//   新生成OP_GETTABLE或OP_GETTABUP指令 指令参数A为reg 参数B为表位置 参数C为key位置
// VVARARG
//   修改原info存放指令修改参数A为reg,参数B为2
// VCALL
//   如果原指令参数A与reg不同生成一条OP_MOVE指令移动到reg
// VJMP
//   对表达式不做任何修改 不做任何操作
static void discharge2reg (FuncState *fs, expdesc *e, int reg) {
  // 处理掉VLOCAL VUPVAL VINDEXED VVARARG VCALL这几个类型
  luaK_dischargevars(fs, e);
  switch (e->k) {
    // nil false 常量 浮点数 整数这些类型直接生成指令即可
    case VNIL: {
      luaK_nil(fs, reg, 1);
      break;
    }
    case VFALSE: case VTRUE: {
      luaK_codeABC(fs, OP_LOADBOOL, reg, e->k == VTRUE, 0);
      break;
    }
    // 常量如字符串在识别时已经写入了常量表,只需要生成一条OP_LOADK指令
    case VK: {
      luaK_codek(fs, reg, e->u.info);
      break;
    }
    // 浮点数和整数都是先加入常量表,再生成OP_LOADK指令
    case VKFLT: {
      luaK_codek(fs, reg, luaK_numberK(fs, e->u.nval));
      break;
    }
    case VKINT: {
      luaK_codek(fs, reg, luaK_intK(fs, e->u.ival));
      break;
    }
    // 需要重定向的指令
    case VRELOCABLE: {
      // 回填之前生成的指令的A参数
      Instruction *pc = &getinstruction(fs, e);
      SETARG_A(*pc, reg);  /* instruction will put result in 'reg' */
      break;
    }
    // 没有重定向的指令 不需要回填
    case VNONRELOC: {
      if (reg != e->u.info)                           // 已经设置存放的位置和传入的位置不同
        luaK_codeABC(fs, OP_MOVE, reg, e->u.info, 0); // 新增一条OP_MOVE指令移动到reg的位置
      break;
    }
    default: {  // 只有可能是VJMP类型 VJMP类型不要修改为VNONRELOC类型
      lua_assert(e->k == VJMP);
      return;  /* nothing to do... */
    }
  }
  e->u.info = reg;  // info保存这些值被保存的位置
  e->k = VNONRELOC; // 现在这些值都已经就绪都是固定位置了
}


/*
** Ensures expression value is in any register.
*/
// 用来将不是VJM类型表达式结果存入任意一个寄存器,是对discharge2reg的封装,不需要指定寄存器
// 函数名any(任意)的含义是最终保存寄存器位置不确定
// 因为VNONRELOC类型存在原始位置,不做操作 其他类型存入当前freereg位置
static void discharge2anyreg (FuncState *fs, expdesc *e) {
  // VNONRELOC类型已经保存在寄存器中,不需要操作
  if (e->k != VNONRELOC) {  /* no fixed register yet? */
    // freereg后移1位
    luaK_reserveregs(fs, 1);  /* get a register */
    // freereg-1是当前空闲的一个位置
    discharge2reg(fs, e, fs->freereg-1);  /* put value there */
  }
}


// 生成一条OP_LOADBOOL指令
// A是OP_LOADBOOL指令的参数A
// b是OP_LOADBOOL指令的参数B 是布尔类型值0或1
// c是OP_LOADBOOL指令的参数C 是布尔类型值0或1
// OP_LOADBOOL指令含义 R(A) := (Bool)B; if(C) pc++
static int code_loadbool (FuncState *fs, int A, int b, int jump) {
  luaK_getlabel(fs);  /* those instructions may be jump targets ?? */
  return luaK_codeABC(fs, OP_LOADBOOL, A, b, jump);
}


/*
** check whether list has any jump that do not produce a value
** or produce an inverted value
*/
// list回填链中指令的控制指令存在不是OP_TESTSET指令的指令返回1
// list回填链中指令的控制指令都是OP_TESTSET指令返回0
// 除了OP_TESTSET指令外,其余测试指令OP_EQ等,都需要生成OP_LOADBOOL指令加载返回的布尔类型值来做处理
static int need_value (FuncState *fs, int list) {
  // 遍历list回填链
  for (; list != NO_JUMP; list = getjump(fs, list)) {
    Instruction i = *getjumpcontrol(fs, list);
    if (GET_OPCODE(i) != OP_TESTSET) return 1;
  }
  return 0;  /* not found */
}


/*
** Ensures final expression result (including results from its jump
** lists) is in register 'reg'.
** If expression has jumps, need to patch these jumps either to
** its final position or to "load" instructions (for those tests
** that do not produce values).
*/
// 将表达式的值放到寄存器reg中
// 所有类型表达式e->k = VNONRELOC;e->u.info = reg;
// 回填了表达式的truelist和falselist
static void exp2reg (FuncState *fs, expdesc *e, int reg) {
  // 将表达式e值解析出来,赋值到寄存器reg中
  discharge2reg(fs, e, reg);
  // 是VJMP类型的表达式,将它自己带的跳转指令加入到它的truelist中
  if (e->k == VJMP)  /* expression itself is a test? */
    luaK_concat(fs, &e->t, e->u.info);  /* put this jump in 't' list */
  // 对于hasjumps的表达式需要对两个链(truelist,falselist)进行回填
  //   控制指令是OP_TESTSET指令,使用reg回填OP_TESTSET,使用表达式结束位置回填OP_JMP
  //   控制指令不是OP_TESTSET,使用加载true或false的OP_LOADBOOL指令回填对应的OP_JMP指令
  // VJMP类型
  //   生成两个OP_LOADBOOL指令
  //   然后回填truelist和falselist
  // 其他类型
  //   生成一条OP_JMP指令跳转到表达式结束
  //   生成两个OP_LOADBOOL指令
  //   然后回填truelist和falselist
  // VJMP类型没有在discharge2reg中处理,因为最终结果是true或者false,在这里通过OP_LOADBOOL指令统一解决
  // 其他类型在discharge2reg中加载了结果,这里需要一条额外的跳转到表达式结束的指令
  if (hasjumps(e)) {  // 判断是否有truelist或者falselist
    // final表示的表达式的下一个指令位置
    int final;  /* position after whole expression */
    int p_f = NO_JUMP;  /* position of an eventual LOAD false */
    int p_t = NO_JUMP;  /* position of an eventual LOAD true */
    // truelist和falselist中有不是OP_TESTSET的控制指令 这样就需要生成OP_LOADBOOL指令
    if (need_value(fs, e->t) || need_value(fs, e->f)) {
      // 非VJMP类型的需要一条额外的跳转到表达式结束的指令
      int fj = (e->k == VJMP) ? NO_JUMP : luaK_jump(fs);  // ??
      // 参数C是true保证执行了这个指令不执行下面加载true的指令
      p_f = code_loadbool(fs, reg, 0, 1);  // 生成加载false的指令
      p_t = code_loadbool(fs, reg, 1, 0);  // 生成加载true的指令
      // 这一行不能在生成生成loadbool之前不然jpc就被loadbool回填了
      luaK_patchtohere(fs, fj);  // 新生成的跳转指令加入到jpc上
    }
    final = luaK_getlabel(fs);
    // 回填falselist和truelist,这里vtarget和dtarget不同
    // 控制指令是OP_TESTSET的跳转指令用final回填 其他的用p_f或p_t回填
    // 因为OP_TESTSET不需要执行OP_LOADBOOL了,它自己就加载了结果
    patchlistaux(fs, e->f, final, reg, p_f);
    patchlistaux(fs, e->t, final, reg, p_t);
  }
  e->f = e->t = NO_JUMP;
  e->u.info = reg;
  e->k = VNONRELOC;
}


/*
** Ensures final expression result (including results from its jump
** lists) is in next available register.
*/
// 将表达式结果保存到最后一个寄存器上,也就是调用后freereg的结果
void luaK_exp2nextreg (FuncState *fs, expdesc *e) {
  luaK_dischargevars(fs, e);  // 处理VLOCAL,VUPVAL等类型
  // 如果当前表达式已经保存到寄存器中临时值,先释放掉,下面再保存一次
  freeexp(fs, e);
  // fs->freereg后移1位 freereg-1就是一个空闲的位置
  luaK_reserveregs(fs, 1);
  exp2reg(fs, e, fs->freereg - 1);
}


/*
** Ensures final expression result (including results from its jump
** lists) is in some (any) register and return that register.
*/
// 确保将表达式e的值放入某个寄存器 返回寄存器的位置
// VLOCAL,VCALL,VNONRELOC类型且不存在跳转链直接返回寄存器位置
int luaK_exp2anyreg (FuncState *fs, expdesc *e) {
  luaK_dischargevars(fs, e);
  if (e->k == VNONRELOC) {  /* expression already has a register? */
    if (!hasjumps(e))  /* no jumps? */
      return e->u.info;  /* result is already in a register */
    // hasjumps且是存在寄存器的临时值,直接将表达式结果写入临时值
    // 也可以直接调用luaK_exp2nextreg效果一样,但是会增加freereg一增一减的操作
    if (e->u.info >= fs->nactvar) {  /* reg. is not a local? */
      // 调用exp2reg处理truelist或者falselist中的值存到info中
      exp2reg(fs, e, e->u.info);  /* put final result in it */
      return e->u.info;
    }
    // 执行到这里说明是局部变量且有truelist或者falselist
    // 不能返回局部变量所在的寄存器,要将结果保存到新的寄存器
    // 如果局部变量也直接调用exp2reg那么就会修改局部变量的值了
  }
  // 正常情况直接存放到下一个寄存器中
  luaK_exp2nextreg(fs, e);  /* otherwise, use next available register */
  return e->u.info;
}


/*
** Ensures final expression result is either in a register or in an
** upvalue.
*/
// 把表达式e存到寄存器或者在upvalue
// 是upvalue且没有truelist和falselist的表达式不处理
void luaK_exp2anyregup (FuncState *fs, expdesc *e) {
  if (e->k != VUPVAL || hasjumps(e))
    luaK_exp2anyreg(fs, e);
}


/*
** Ensures final expression result is either in a register or it is
** a constant.
*/
// 没有跳转链等价于luaK_dischargevars
// 有跳转链调用luaK_exp2anyreg
void luaK_exp2val (FuncState *fs, expdesc *e) {
  if (hasjumps(e))
    luaK_exp2anyreg(fs, e);
  else
    luaK_dischargevars(fs, e);
}


/*
** Ensures final expression result is in a valid R/K index
** (that is, it is either in a register or in 'k' with an index
** in the range of R/K indices).
** Returns R/K index.
*/
// 表达式e是常量 返回值在常量表中的位置(第8位会标记成1) 并不是e->u.info
// 表达式e不是常量 返回值在寄存器中的位置
int luaK_exp2RK (FuncState *fs, expdesc *e) {
  luaK_exp2val(fs, e);
  // true,false,nil,int,flt类型先存入常量表,设置info为常量表位置
  //   再设置类型为VK,返回标记第8位后的info值
  // 本来就是VK类型直接返回标记后的info值
  switch (e->k) {  /* move constants to 'k' */
    case VTRUE: e->u.info = boolK(fs, 1); goto vk;
    case VFALSE: e->u.info = boolK(fs, 0); goto vk;
    case VNIL: e->u.info = nilK(fs); goto vk;
    case VKINT: e->u.info = luaK_intK(fs, e->u.ival); goto vk;
    case VKFLT: e->u.info = luaK_numberK(fs, e->u.nval); goto vk;
    case VK:
     vk:
      e->k = VK;
      if (e->u.info <= MAXINDEXRK)  /* constant fits in 'argC'? */
        return RKASK(e->u.info);
      else break;
    default: break;
  }
  /* not a constant in the right range: put it in a register */
  // 执行到这里说明不是常量 返回寄存器的位置
  return luaK_exp2anyreg(fs, e);
}


/*
** Generate code to store result of expression 'ex' into variable 'var'.
*/
// 将表达式ex的结果存放到表达式var中
void luaK_storevar (FuncState *fs, expdesc *var, expdesc *ex) {
  switch (var->k) {
    // 直接保存ex的值到var->u.info即可
    case VLOCAL: {
      freeexp(fs, ex);
      exp2reg(fs, ex, var->u.info);  /* compute 'ex' into proper place */
      return;
    }
    // 先保存ex到寄存器,再使用OP_SETUPVAL指令设置到var上
    case VUPVAL: {
      int e = luaK_exp2anyreg(fs, ex);
      luaK_codeABC(fs, OP_SETUPVAL, e, var->u.info, 0);
      break;
    }
    // 生成OP_SETTABLE或者OP_SETTABUP指令设置表项的值
    case VINDEXED: {
      OpCode op = (var->u.ind.vt == VLOCAL) ? OP_SETTABLE : OP_SETTABUP;
      // OP_SETTABLE,OP_SETTABLE的B参数是RK,所以这里调用luaK_exp2RK
      int e = luaK_exp2RK(fs, ex);
      luaK_codeABC(fs, op, var->u.ind.t, var->u.ind.idx, e);
      break;
    }
    default: lua_assert(0);  /* invalid var kind to store */
  }
  freeexp(fs, ex);
}


/*
** Emit SELF instruction (convert expression 'e' into 'e:key(e,').
*/
// a:b('x')与a.b(a,'x')等价
// 表达式e就代表表a,表达式key代表字符串常量'b'
// 保存e和key,生成OP_SELF指令
void luaK_self (FuncState *fs, expdesc *e, expdesc *key) {
  int ereg;
  luaK_exp2anyreg(fs, e);
  ereg = e->u.info;  /* register where 'e' was placed */
  freeexp(fs, e);
  // e开始表示表a,现在代表a.b
  // e->u.info经过OP_SELF指令保存被调用的函数a.b
  e->u.info = fs->freereg;  /* base register for op_self */
  e->k = VNONRELOC;  /* self expression has a fixed register */
  // OP_SELF指令需要两个寄存器,保存被调用的函数和参数self
  luaK_reserveregs(fs, 2);  /* function and 'self' produced by op_self */
  luaK_codeABC(fs, OP_SELF, e->u.info, ereg, luaK_exp2RK(fs, key));
  freeexp(fs, key);
}


/*
** Negate condition 'e' (where 'e' is a comparison).
*/
// 将表达e的控制指令(OP_EQ,OP_LT,OP_LE这三种类型)的参数A取反
static void negatecondition (FuncState *fs, expdesc *e) {
  Instruction *pc = getjumpcontrol(fs, e->u.info);
  // pc代表的指令一定是 OP_EQ,OP_LT,OP_LE这三种
  lua_assert(testTMode(GET_OPCODE(*pc)) && GET_OPCODE(*pc) != OP_TESTSET &&
                                           GET_OPCODE(*pc) != OP_TEST);
  // 参数A控制了指令的真假值 将A取反就相当于加了not
  SETARG_A(*pc, !(GETARG_A(*pc)));
}


/*
** Emit instruction to jump if 'e' is 'cond' (that is, if 'cond'
** is true, code will jump if 'e' is true.) Return jump position.
** Optimize when 'e' is 'not' something, inverting the condition
** and removing the 'not'.
*/
// 当表达式与cond相等时执行OP_JMP指令跳转
// 生成OP_TESTSET指令和OP_JMP指令
// 将表达式e拷贝进寄存器,如果e的值==cond,那么就执行生成的跳转语句,否则pc++跳过跳转语句
static int jumponcond (FuncState *fs, expdesc *e, int cond) {
  // 这里优化了OP_NOT指令
  // 不优化的情况下,最终生成
  // NOT A B  先取反存到一个寄存器A里
  // TESTSET A cond  将寄存器A与cond比较,为真执行JMP指令
  // JMP 
  // 优化后
  // TEST B !cond  将寄存器B与!cond比较,为真执行JMP指令
  // JMP
  // 可以发现优化后省了一次将B寄存器位置的取反存到A寄存器的过程
  if (e->k == VRELOCABLE) {
    Instruction ie = getinstruction(fs, e);
    if (GET_OPCODE(ie) == OP_NOT) {
      // 删除OP_NOT指令
      fs->pc--;  /* remove previous OP_NOT */
      // 如果R(A)==!cond执行跳转语句
      return condjump(fs, OP_TEST, GETARG_B(ie), 0, !cond);
    }
    /* else go through */
  }
  // 不需要优化,正常的执行过程
  // 将表达式e的值存入寄存器
  discharge2anyreg(fs, e);
  freeexp(fs, e);
  // 生成OP_TESTSET指令和OP_JMP指令
  // R(e->u.info)==cond就执行跳转语句
  return condjump(fs, OP_TESTSET, NO_REG, e->u.info, cond);
}


// 以下的goiftrue和goiffalse是往表达式的truelist和falselist添加新指令的唯一途径
// exp2reg中对于VJMP类型将自带跳转指令加入truelist,但是在函数内部就回填了并清空了
// luaK_posfix中也修改了truelist或falselist但是是从e1合并到e2,不包含引入新指令
//
// goiftrue代表测试指令为真继续执行,为假执行跳转指令
// goiffalse与goiftrue相反,为真执行跳转指令
//
// 对于goiftrue就是要清空表达式的truelist,用下一条指令回填truelist
// 对于goiffalse就是要清空falselist,用下一条指令回填falselist
/*
** Emit code to go through if 'e' is true, jump otherwise.
*/
// 当表达式e为真的时候继续执行,为假执行跳转
// 所以truelist加入jpc,也就是说truelist中需要回填的地址现在已经知道了就是下一条指令
// 根据各种类型生成跳转指令并加入falselist,因为现在不确定false跳转位置
// 表达式e是VJMP类型,自带了跳转指令
//   VJMP本身是goiffalse,所以将控制指令参数A取反,自带跳转指令加入falselist,truelist加入到fs->jpc后被清空
// 表达式e是VK,VKFLT,VKINT,VTRUE等一定为真,truelist加入到fs->jpc中后被清空,不生成任何指令
// 其他类型表达式生成加载变量的指令和一条跳转指令,最终表达式为真继续执行(不执行跳转指令,pc++)
void luaK_goiftrue (FuncState *fs, expdesc *e) {
  // pc是新增或VJMP类型自带跳转指令的位置,pc将会被加入到表达e的falselist中
  int pc;  /* pc of new jump */
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VJMP: {  /* condition? */
      // 将控制指令的参数A取反
      // 本来控制指令条件为真的时候会执行下一条OP_JMP指令 现在条件为假才会执行
      negatecondition(fs, e);  /* jump when it is false */
      // 保存OP_JMP指令的位置
      pc = e->u.info;  /* save jump position */
      break;
    }
    // 一定为真什么也不做
    case VK: case VKFLT: case VKINT: case VTRUE: {
      pc = NO_JUMP;  /* always true; do nothing */
      break;
    }
    // 可能为VNIL,VFALSE,VNONRELOC,VRELOCABLE
    default: {
      // 这里0的意思是如果表达式e的值为false就执行生成OP_JMP指令,否则pc++,跳过跳转指令
      pc = jumponcond(fs, e, 0);  /* jump when false */
      break;
    }
  }
  // 在表达式e的falselist中新增pc,也就是OP_JMP指令
  // 也就是这条跳转指令需要回填将来表达式为假跳转的位置
  luaK_concat(fs, &e->f, pc);  /* insert new jump in false list */
  // 将truelist加入到fs->jpc中
  // 表达式为真,所有truelist中的指令回填下一条生成指令的位置
  luaK_patchtohere(fs, e->t);  /* true list jumps to here (to go through) */
  e->t = NO_JUMP;
}


/*
** Emit code to go through if 'e' is false, jump otherwise.
*/
void luaK_goiffalse (FuncState *fs, expdesc *e) {
  int pc;  /* pc of new jump */
  luaK_dischargevars(fs, e);
  switch (e->k) {
    // VJMP本身就是goiffalse,不需要像goiftrue中取反
    case VJMP: {
      pc = e->u.info;  /* already jump if true */
      break;
    }
    case VNIL: case VFALSE: {
      pc = NO_JUMP;  /* always false; do nothing */
      break;
    }
    default: {
      pc = jumponcond(fs, e, 1);  /* jump if true */
      break;
    }
  }
  luaK_concat(fs, &e->t, pc);  /* insert new jump in 't' list */
  // 使用下一条指令回填falselist,并清空falselist
  luaK_patchtohere(fs, e->f);  /* false list jumps to here (to go through) */
  e->f = NO_JUMP;
}


/*
** Code 'not e', doing constant folding.
*/
// 执行'not e'操作
// VNIL,VFALSE类型直接修改为VTRUE
// VK,VKFLT,VKINT,VTRUE类型修改为VFALSE
// VJMP类型将控制指令参数A取反
// VRELOCABLE,VNONRELOC生成OP_NOT指令
static void codenot (FuncState *fs, expdesc *e) {
  luaK_dischargevars(fs, e);
  switch (e->k) {
    case VNIL: case VFALSE: {
      e->k = VTRUE;  /* true == not nil == not false */
      break;
    }
    // not 常量,浮点数,整数,true都是false 特别的not 0也是false
    case VK: case VKFLT: case VKINT: case VTRUE: {
      e->k = VFALSE;  /* false == not "x" == not 0.5 == not 1 == not true */
      break;
    }
    // VJMP类型的表达式info指向跳转指令
    // info前一个指令(控制指令)是OP_EQ,OP_LT或OP_LE指令
    // 对VJMP表达式取非,只需要将参数A取反
    case VJMP: {  // 将控制指令参数A取反
      negatecondition(fs, e);
      break;
    }
    case VRELOCABLE:
    case VNONRELOC: {
      // 先把表达式的值放到寄存器里
      discharge2anyreg(fs, e);
      freeexp(fs, e);
      // 生成OP_NOT指令 参数A需要回填设置表达式类型为VRELOCABLE
      e->u.info = luaK_codeABC(fs, OP_NOT, 0, e->u.info, 0);
      e->k = VRELOCABLE;
      break;
    }
    default: lua_assert(0);  /* cannot happen */
  }
  /* interchange true and false lists */
  // 交换 truelist和falselist
  { int temp = e->f; e->f = e->t; e->t = temp; }
  // 原来表达式产生的值没有用了 如果有OP_TESTSET指令全部改成OP_TEST
  removevalues(fs, e->f);  /* values are useless when negated */
  removevalues(fs, e->t);
}


/*
** Create expression 't[k]'. 't' must have its final result already in a
** register or upvalue.
*/
// 输入时表达式t代表了被查询了表
// 结束时设置表达式t的类型为VINDEXED,初始化表达式t的ind属性
void luaK_indexed (FuncState *fs, expdesc *t, expdesc *k) {
  lua_assert(!hasjumps(t) && (vkisinreg(t->k) || t->k == VUPVAL));
  // 保存表t所在的位置
  t->u.ind.t = t->u.info;  /* register or upvalue index */
  // 保存k所在位置 可能在寄存器也可能在常量表 常量的话第8位被标记为1
  t->u.ind.idx = luaK_exp2RK(fs, k);  /* R/K index for key */
  // 标记该表是在upvalue还是局部变量
  t->u.ind.vt = (t->k == VUPVAL) ? VUPVAL : VLOCAL;
  t->k = VINDEXED;
}


/*
** Return false if folding can raise an error.
** Bitwise operations need operands convertible to integers; division
** operations cannot have 0 as divisor.
*/
// 判断在v1和v2之间执行操作op是否可性(v1和v2现在一定是数字)
// 返回1可以执行op 返回0不能执行op
static int validop (int op, TValue *v1, TValue *v2) {
  switch (op) {
    // 只能在两个整数间进行的操作
    case LUA_OPBAND: case LUA_OPBOR: case LUA_OPBXOR:
    case LUA_OPSHL: case LUA_OPSHR: case LUA_OPBNOT: {  /* conversion errors */
      lua_Integer i;
      return (tointeger(v1, &i) && tointeger(v2, &i));
    }
    // 除法v2不能是0
    case LUA_OPDIV: case LUA_OPIDIV: case LUA_OPMOD:  /* division by 0 */
      return (nvalue(v2) != 0);
    // 其他操作都是有效的
    default: return 1;  /* everything else is valid */
  }
}


/*
** Try to "constant-fold" an operation; return 1 iff successful.
** (In this case, 'e1' has the final result.)
*/
// 计算表达式e1和e2在op操作下的结果
// 该函数用于合并可以直接计算出来的结果,例如1+1,-2,3*4等
// 返回1 计算成功e1保存最终运算结果
// 返回0 计算失败
static int constfolding (FuncState *fs, int op, expdesc *e1,
                                                const expdesc *e2) {
  TValue v1, v2, res;
  // 判断两个表达式之间能否执行操作op 将值写入v1和v2
  if (!tonumeral(e1, &v1) || !tonumeral(e2, &v2) || !validop(op, &v1, &v2))
    return 0;  /* non-numeric operands or not safe to fold */
  // 执行运算 结果写入res
  luaO_arith(fs->ls->L, op, &v1, &v2, &res);  /* does operation */
  if (ttisinteger(&res)) {  // 计算结果是整数
    e1->k = VKINT;
    e1->u.ival = ivalue(&res);
  }
  else {  /* folds neither NaN nor 0.0 (to avoid problems with -0.0) */
    lua_Number n = fltvalue(&res);
    // 避免nan和-0.0
    if (luai_numisnan(n) || n == 0)
      return 0;
    e1->k = VKFLT;
    e1->u.nval = n;
  }
  return 1;
}


/*
** Emit code for unary expressions that "produce values"
** (everything but 'not').
** Expression to produce final result will be encoded in 'e'.
*/
// 为一元操作符(除了not)生成指令,例如OP_UNM,OP_BNOT,OP_LEN
static void codeunexpval (FuncState *fs, OpCode op, expdesc *e, int line) {
  // 将表达式放入寄存器
  int r = luaK_exp2anyreg(fs, e);  /* opcodes operate only on registers */
  freeexp(fs, e);
  // 生成指令
  e->u.info = luaK_codeABC(fs, op, 0, r, 0);  /* generate opcode */
  e->k = VRELOCABLE;  /* all those operations are relocatable */
  luaK_fixline(fs, line);  // 添加生成指令所在行的信息
}


/*
** Emit code for binary expressions that "produce values"
** (everything but logical operators 'and'/'or' and comparison
** operators).
** Expression to produce final result will be encoded in 'e1'.
** Because 'luaK_exp2RK' can free registers, its calls must be
** in "stack order" (that is, first on 'e2', which may have more
** recent registers to be released).
*/
// 为除了and/or/比较运算的所有二元运算生成指令
// 例如+,-,*,/等等
// 先将两个操作数保存到RK,释放两个操作数的位置
// 生成计算指令,两个操作数的位置就是刚才保存的位置
// 计算指令的结果不知道保存到哪里所以最终类型是VRELOCABLE
static void codebinexpval (FuncState *fs, OpCode op,
                           expdesc *e1, expdesc *e2, int line) {
  // 先将两个操作数保存到RK中
  int rk2 = luaK_exp2RK(fs, e2);  /* both operands are "RK" */
  int rk1 = luaK_exp2RK(fs, e1);
  // 保存的操作数只是中间值,最终不需要
  freeexps(fs, e1, e2);
  e1->u.info = luaK_codeABC(fs, op, 0, rk1, rk2);  /* generate opcode */
  e1->k = VRELOCABLE;  /* all those operations are relocatable */
  luaK_fixline(fs, line);
}


/*
** Emit code for comparisons.
** 'e1' was already put in R/K form by 'luaK_infix'.
*/
// 根据操作符对表达式e1和e2生成比较指令OP_EQ,OP_LT,OP_LE和一条跳转指令
// 输入是e1已经在RK中,该函数也将e2存入RK中
// 如果e1 op e2为真那么就执行跳转语句, 例如a<b如果为真就执行下一条OP_JMP
// 修改e1的类型为VJMP,info指向生成的跳转指令
// 操作符包括 ~=,>,>=,==,<,<=
// e1已经通过luaK_infix保存在寄存器或者常量表中
static void codecomp (FuncState *fs, BinOpr opr, expdesc *e1, expdesc *e2) {
  // e1->k == VK说明保存在常量表中
  // e1->k == VNONRELOC说明保存在寄存器中
  // 获取e1实际值存放的位置
  int rk1 = (e1->k == VK) ? RKASK(e1->u.info)
                          : check_exp(e1->k == VNONRELOC, e1->u.info);
  // 将e2也存进RK中 获取位置
  int rk2 = luaK_exp2RK(fs, e2);
  freeexps(fs, e1, e2);
  switch (opr) {
    case OPR_NE: {  /* '(a ~= b)' ==> 'not (a == b)' */
      // a~=b转换成not(a==b) 设置参数A为0即可
      e1->u.info = condjump(fs, OP_EQ, 0, rk1, rk2);
      break;
    }
    case OPR_GT: case OPR_GE: {
      /* '(a > b)' ==> '(b < a)';  '(a >= b)' ==> '(b <= a)' */
      OpCode op = cast(OpCode, (opr - OPR_NE) + OP_EQ);
      // 对于a>b和a>=b类型 转换成a<b和a<=b并交换rk2和rk1的位置就等价
      e1->u.info = condjump(fs, op, 1, rk2, rk1);  /* invert operands */
      break;
    }
    default: {  /* '==', '<', '<=' use their own opcodes */
      // 计算出来操作码
      OpCode op = cast(OpCode, (opr - OPR_EQ) + OP_EQ);
      e1->u.info = condjump(fs, op, 1, rk1, rk2);
      break;
    }
  }
  // 这个函数合并了e1和e2表达式 例如1<2 e1就是1,e2就是2,操作符是<
  // 最终结束后合并成一个表达式,放在e1中,类型为VJMP
  e1->k = VJMP;
}


/*
** Aplly prefix operation 'op' to expression 'e'.
*/
// 对表达式e执行它的前缀操作 例如not,-,~,#等
// VINT,VFLT类型直接计算出来结果保存到表达式中
// 其他类型生成对应的指令,info保存指令位置,类型修改为VRELOCABLE因为生成指令参数A没确定
void luaK_prefix (FuncState *fs, UnOpr op, expdesc *e, int line) {
  static const expdesc ef = {VKINT, {0}, NO_JUMP, NO_JUMP};
  switch (op) {
    // 处理-和~ ef当作假的第二个操作数
    case OPR_MINUS: case OPR_BNOT:  /* use 'ef' as fake 2nd operand */
      // LUA_OPUNM为12,OPR_MINUS为0
      // constfolding函数接收的是LUA_OPUNM
      // OPR_MINUS和OPR_BNOT加上LUA_OPUNM就能转换
      // 计算成功e保存结果
      if (constfolding(fs, op + LUA_OPUNM, e, &ef))
        break;
      /* FALLTHROUGH */
      // 上面合并失败的话和OPR_LEN处理方法一样,调用codeunexpval
    case OPR_LEN:  // #计算长度
      // op+OP_UNM转换成一元运算的指令 OP_UNM,OP_BNOT,OP_LEN
      codeunexpval(fs, cast(OpCode, op + OP_UNM), e, line);
      break;
    // 处理not
    case OPR_NOT: codenot(fs, e); break;
    default: lua_assert(0);
  }
}


/*
** Process 1st operand 'v' of binary operation 'op' before reading
** 2nd operand.
*/
// 现在已经读入了第一个操作数v和操作符op,在读入第二个操作数前对第一个操作数进行操作
void luaK_infix (FuncState *fs, BinOpr op, expdesc *v) {
  switch (op) {
    case OPR_AND: {
      // 对于and运算只有第一个操作符为true的时候才去执行第二个操作符
      // 所以这里是goiftrue
      luaK_goiftrue(fs, v);  /* go ahead only if 'v' is true */
      break;
    }
    case OPR_OR: {
      // 对于or运算只有第一个操作符为false才执行第二个操作符
      luaK_goiffalse(fs, v);  /* go ahead only if 'v' is false */
      break;
    }
    case OPR_CONCAT: {  // 连接操作将操作数一放入寄存器
      luaK_exp2nextreg(fs, v);  /* operand must be on the 'stack' */
      break;
    }
    // 针对加减这些运算如果识别到的直接就是VKINT和VKFLT不做操作
    // 不是这两种类型需要存到RK中,例如1+2这里就不对表达式操作
    // 如果是a+b这里就需要将a存到寄存器
    // 因为1+2最终的识别结果就是VKINT保存值是3,不需要生成任何指令
    // a+b需要生成一条OP_ADD指令
    case OPR_ADD: case OPR_SUB:
    case OPR_MUL: case OPR_DIV: case OPR_IDIV:
    case OPR_MOD: case OPR_POW:
    case OPR_BAND: case OPR_BOR: case OPR_BXOR:
    case OPR_SHL: case OPR_SHR: {
      if (!tonumeral(v, NULL))
        luaK_exp2RK(fs, v);  // 不能转换成数字 将表达式v的值放入RK
      /* else keep numeral, which may be folded with 2nd operand */
      // 能转换成数字 这两个操作数可能会被合并
      break;
    }
    default: {
      luaK_exp2RK(fs, v);  // 其他操作符直接把值放入RK中
      break;
    }
  }
}


/*
** Finalize code for binary operation, after reading 2nd operand.
** For '(a .. b .. c)' (which is '(a .. (b .. c))', because
** concatenation is right associative), merge second CONCAT into first
** one.
*/
// 为二元操作符生成最终的指令
void luaK_posfix (FuncState *fs, BinOpr op,
                  expdesc *e1, expdesc *e2, int line) {
  // 在luaK_infix已经处理过e1
  switch (op) {
    // lua的and和or运算符最终返回结果是两个操作数的其中之一,并不是true,false
    // and: 如果e1为假,最终结果是e1,否则最终结果是e2
    // or:  如果e1为真,最终结果是e1,否则最终结果是e2
    // 例如 1 and 10结果是10, 1 or 10结果是1
    case OPR_AND: {
      // 针对and操作,在luaK_infix已经将truelist加到fs->jpc上了
      lua_assert(e1->t == NO_JUMP);  /* list closed by 'luaK_infix' */
      luaK_dischargevars(fs, e2);
      // 将e1的falselist加入到e2的falselist后面
      luaK_concat(fs, &e2->f, e1->f);
      // e1拷贝为e2
      *e1 = *e2;
      break;
    }
    case OPR_OR: {
      // 针对or操作,在luaK_infix已经将truelist加到fs->jpc上了
      lua_assert(e1->f == NO_JUMP);  /* list closed by 'luaK_infix' */
      luaK_dischargevars(fs, e2);
      // 合并truelist 将e2拷贝进e1
      luaK_concat(fs, &e2->t, e1->t);
      *e1 = *e2;
      break;
    }
    // .. 连接操作可以进行优化
    // 例如 1 .. 2 .. 3,可以只生成一条指令就将三者合并
    // 这里e1是1,e2是2 .. 3,e2已经有了CONCAT指令,只需要设置参数B减一,避免再生成一条合并指令
    case OPR_CONCAT: {  // e1一定在寄存器中
      // e2有跳转链放入寄存器,否则只discharge
      luaK_exp2val(fs, e2);
      // 如果e2也是一个连接操作,可以进行优化
      if (e2->k == VRELOCABLE &&
          GET_OPCODE(getinstruction(fs, e2)) == OP_CONCAT) {
        // 这里e2有 CONCAT A B C,代表连接R(B)到R(C)
        // e1存在B-1里,显然这里可以直接修改指令为 CONCAT A B-1 C,避免再生成一条合并指令
        lua_assert(e1->u.info == GETARG_B(getinstruction(fs, e2))-1);
        freeexp(fs, e1);
        // 修改参数B
        SETARG_B(getinstruction(fs, e2), e1->u.info);
        e1->k = VRELOCABLE; e1->u.info = e2->u.info;
      }
      // 正常情况的合并
      else {
        // 将e2保存到寄存器
        luaK_exp2nextreg(fs, e2);  /* operand must be on the 'stack' */
        // 生成OP_CONCAT指令
        codebinexpval(fs, OP_CONCAT, e1, e2, line);
      }
      break;
    }
    // 针对加减等运算先尝试直接合并结果例如1+1合并为2
    // 不能合并的再生成指令,变成VRELOCABLE类型
    case OPR_ADD: case OPR_SUB: case OPR_MUL: case OPR_DIV:
    case OPR_IDIV: case OPR_MOD: case OPR_POW:
    case OPR_BAND: case OPR_BOR: case OPR_BXOR:
    case OPR_SHL: case OPR_SHR: {
      // e1可能是VKINT,VKFLT或者已经保存到RK中了
      if (!constfolding(fs, op + LUA_OPADD, e1, e2))
        codebinexpval(fs, cast(OpCode, op + OP_ADD), e1, e2, line);
      break;
    }
    case OPR_EQ: case OPR_LT: case OPR_LE:
    case OPR_NE: case OPR_GT: case OPR_GE: {
      // 生成OP_EQ,OP_LT,OP_LE指令之一,以及一条跳转指令
      codecomp(fs, op, e1, e2);
      break;
    }
    default: lua_assert(0);
  }
}


/*
** Change line information associated with current position.
*/
void luaK_fixline (FuncState *fs, int line) {
  fs->f->lineinfo[fs->pc - 1] = line;
}


/*
** Emit a SETLIST instruction.
** 'base' is register that keeps table;
** 'nelems' is #table plus those to be stored now;
** 'tostore' is number of values (in registers 'base + 1',...) to add to
** table (or LUA_MULTRET to add up to stack top).
*/
// 生成OP_SETLIST指令
// 现在栈顶的情况是 T A A A,T是被设置的表,A是设置的值
// base是表的位置,nelems是表数组部分现有的个数加tostore,tostore是即将加入表中的个数
void luaK_setlist (FuncState *fs, int base, int nelems, int tostore) {
  int c =  (nelems - 1)/LFIELDS_PER_FLUSH + 1;  // 计算要设置到第几个块
  int b = (tostore == LUA_MULTRET) ? 0 : tostore;  // 是LUA_MULTRET说明不确定设置几个值
  // tostore要么是-1 要么是1-50不能是0
  lua_assert(tostore != 0 && tostore <= LFIELDS_PER_FLUSH);
  if (c <= MAXARG_C)
    luaK_codeABC(fs, OP_SETLIST, base, b, c);  // 生成指令
  else if (c <= MAXARG_Ax) {  // 参数C在一条指令存不开 使用OP_EXTRAARG指令存参数C
    luaK_codeABC(fs, OP_SETLIST, base, b, 0);  // C是0用来标记C存在下一个指令中
    codeextraarg(fs, c);
  }
  else  // 实在存不下 这表太大了
    luaX_syntaxerror(fs->ls, "constructor too long");
  // 释放掉原来存的要设置到表中的值
  fs->freereg = base + 1;  /* free registers with list values */
}

