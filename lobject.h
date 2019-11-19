/*
** $Id: lobject.h,v 2.117 2016/08/01 19:51:24 roberto Exp $
** Type definitions for Lua objects
** See Copyright Notice in lua.h
*/


#ifndef lobject_h
#define lobject_h

/*
  LUA_TNIL              空类型
  LUA_TBOOLEAN          布尔型
  LUA_TLIGHTUSERDATA    轻量级用户类型
  LUA_TNUMBER           数字类型
    LUA_TNUMFLT           浮点数
    LUA_TNUMINT           整数
  LUA_TSTRING           字符串类型
    LUA_TSHRSTR           短字符串
    LUA_TLNGSTR           长字符串
  LUA_TTABLE            表类型
  LUA_TFUNCTION         函数类型
    LUA_TLCL              lua 必包
    LUA_TLCF              轻量级c函数
    LUA_TCCL              c必包
  LUA_TUSERDATA         用户数据类型
  LUA_TTHREAD           线程类型
*/



#include "llimits.h"
#include "lua.h"


// lua.h中定义了lua的基本数据类型
// LUA_NUMTAGS 是基本类型总数，它计的是 9 种，没有算上 LUA_TNONE
// LUA_TNONE 是一个辅助类型标记，它仅供 C API 使用，对 Lua 层它是不可见的，因此也就不计入基本类型当中了
/*
#define LUA_TNONE		(-1)

#define LUA_TNIL		0
#define LUA_TBOOLEAN		1
#define LUA_TLIGHTUSERDATA	2
#define LUA_TNUMBER		3
#define LUA_TSTRING		4
#define LUA_TTABLE		5
#define LUA_TFUNCTION		6
#define LUA_TUSERDATA		7
#define LUA_TTHREAD		8

#define LUA_NUMTAGS		9
*/


/*
** Extra tags for non-values
*/
// LUA_TPROTO是额外的标记,本身不带任何值,不计算在值类型中
#define LUA_TPROTO	LUA_NUMTAGS		/* function prototypes 函数原型*/
#define LUA_TDEADKEY	(LUA_NUMTAGS+1)		/* removed keys in tables 元表中删除的key */

/*
** number of all possible tags (including LUA_TNONE but excluding DEADKEY)
*/
// 算上LUA_TNONE还有LUA_TPROTO总共11个
#define LUA_TOTALTAGS	(LUA_TPROTO + 2)


/*
** tags for Tagged Values have the following use of bits:
** bits 0-3: actual tag (a LUA_T* value)
** bits 4-5: variant bits
** bit 6: whether value is collectable
*/
// lua数据类型使用6位进行标记,0-3位表示真实类型 也就是上面的9种类型
// 4-5位表示上面9种类型的子类型
// 6位表示该类型是否可以进行回收


/*
** LUA_TFUNCTION variants:
** 0 - Lua function
** 1 - light C function
** 2 - regular C function (closure)
*/

/* Variant tags for functions */
// 函数类型分为 lua闭包 轻量级c函数 c函数闭包
#define LUA_TLCL	(LUA_TFUNCTION | (0 << 4))  /* Lua closure */
#define LUA_TLCF	(LUA_TFUNCTION | (1 << 4))  /* light C function */
#define LUA_TCCL	(LUA_TFUNCTION | (2 << 4))  /* C closure */


/* Variant tags for strings */
// 字符串类型分为 短字符串 长字符串
#define LUA_TSHRSTR	(LUA_TSTRING | (0 << 4))  /* short strings */
#define LUA_TLNGSTR	(LUA_TSTRING | (1 << 4))  /* long strings */


/* Variant tags for numbers */
// 数字类型分为 浮点数 整数
#define LUA_TNUMFLT	(LUA_TNUMBER | (0 << 4))  /* float numbers */
#define LUA_TNUMINT	(LUA_TNUMBER | (1 << 4))  /* integer numbers */


/* Bit mark for collectable types */
#define BIT_ISCOLLECTABLE	(1 << 6)

/* mark a tag as collectable */
// 用于标记一个类型为可回收类型
#define ctb(t)			((t) | BIT_ISCOLLECTABLE)


/*
** Common type for all collectable objects
*/
typedef struct GCObject GCObject;


/*
** Common Header for all collectable objects (in macro form, to be
** included in other objects)
*/
// 所有的 GCObject 都有一个相同的数据头，叫作 CommonHeader以宏形式定义出来的
// 使用宏是源于使用上的某种便利 C 语言不支持结构的继承
// 所有的 GCObject 都用一个单向链表串了起来。每个对象都以 tt 来识别其类型。marked 域用于标记清除的工作
#define CommonHeader	GCObject *next; lu_byte tt; lu_byte marked


/*
** Common type has only the common header
*/
struct GCObject {
  CommonHeader;
};




/*
** Tagged Values. This is the basic representation of values in Lua,
** an actual value plus a tag with its type.
*/

/*
** Union of all Lua values
*/
// 这是lua所有数据类型的联合, 使用它来表示lua的数据类型
typedef union Value {
  GCObject *gc;    /* collectable objects */
  void *p;         /* light userdata */
  int b;           /* booleans */
  lua_CFunction f; /* light C functions */
  lua_Integer i;   /* integer numbers */
  lua_Number n;    /* float numbers */
} Value;


#define TValuefields	Value value_; int tt_

// 使用value_表示真实值, tt_标记类型
// tt_是type tag的简写,复合类型，用来表示类型
typedef struct lua_TValue {
  TValuefields;
} TValue;



/* macro defining a nil value */
// 就是0
#define NILCONSTANT	{NULL}, LUA_TNIL


// 取出TValue类型的真实value_值
#define val_(o)		((o)->value_)


/* raw type tag of a TValue */
// 取出原始type tag
#define rttype(o)	((o)->tt_)

/* tag with no variants (bits 0-3) */
// 取出变体部分 只有最后4位 即主类型
#define novariant(x)	((x) & 0x0F)

/* type tag of a TValue (bits 0-3 for tags + variant bits 4-5) */
// 主类型加变体类型
#define ttype(o)	(rttype(o) & 0x3F)

/* type tag of a TValue with no variants (bits 0-3) */
// 取出主类型
#define ttnov(o)	(novariant(rttype(o)))


/* Macros to test type */
#define checktag(o,t)		(rttype(o) == (t))  // 检查主类型加变体类型
#define checktype(o,t)		(ttnov(o) == (t)) // 只检查主类型
#define ttisnumber(o)		checktype((o), LUA_TNUMBER)
#define ttisfloat(o)		checktag((o), LUA_TNUMFLT)
#define ttisinteger(o)		checktag((o), LUA_TNUMINT)
#define ttisnil(o)		checktag((o), LUA_TNIL)
#define ttisboolean(o)		checktag((o), LUA_TBOOLEAN)
#define ttislightuserdata(o)	checktag((o), LUA_TLIGHTUSERDATA)
#define ttisstring(o)		checktype((o), LUA_TSTRING)
#define ttisshrstring(o)	checktag((o), ctb(LUA_TSHRSTR))
#define ttislngstring(o)	checktag((o), ctb(LUA_TLNGSTR))
#define ttistable(o)		checktag((o), ctb(LUA_TTABLE))
#define ttisfunction(o)		checktype(o, LUA_TFUNCTION)
// 闭包有lua闭包和c闭包
// 要检查lua类型是否是闭包 首先必须是LUA_TFUNCTION
// 如果是LUA_TFUNCTION还有三种可能 lua闭包 轻量级c函数 c闭包
// 只有轻量级c闭包倒数第5位是1
// 00 0110 lua闭包
// 01 0110 轻量级c函数
// 10 0110 c闭包
#define ttisclosure(o)		((rttype(o) & 0x1F) == LUA_TFUNCTION)
#define ttisCclosure(o)		checktag((o), ctb(LUA_TCCL))
#define ttisLclosure(o)		checktag((o), ctb(LUA_TLCL))
#define ttislcf(o)		checktag((o), LUA_TLCF)
#define ttisfulluserdata(o)	checktag((o), ctb(LUA_TUSERDATA))
#define ttisthread(o)		checktag((o), ctb(LUA_TTHREAD))
#define ttisdeadkey(o)		checktag((o), LUA_TDEADKEY)


/* Macros to access values */
// 不是调试模式时check_exp没有意义, 以下地方都可以忽略只看第二个参数
#define ivalue(o)	check_exp(ttisinteger(o), val_(o).i)
#define fltvalue(o)	check_exp(ttisfloat(o), val_(o).n)
// 取出lua_Number 如果取出的整数 进行一次强制类型转换
// 正常情况就是long long 转换成double
#define nvalue(o)	check_exp(ttisnumber(o), \
	(ttisinteger(o) ? cast_num(ivalue(o)) : fltvalue(o)))
// 需要进行gc的类型存在 o->value_.gc里
#define gcvalue(o)	check_exp(iscollectable(o), val_(o).gc)
#define pvalue(o)	check_exp(ttislightuserdata(o), val_(o).p)
// 需要取出具体的gc类型需要进行类型转换
#define tsvalue(o)	check_exp(ttisstring(o), gco2ts(val_(o).gc))
#define uvalue(o)	check_exp(ttisfulluserdata(o), gco2u(val_(o).gc))
#define clvalue(o)	check_exp(ttisclosure(o), gco2cl(val_(o).gc))
#define clLvalue(o)	check_exp(ttisLclosure(o), gco2lcl(val_(o).gc))
#define clCvalue(o)	check_exp(ttisCclosure(o), gco2ccl(val_(o).gc))
// 轻量级c函数
#define fvalue(o)	check_exp(ttislcf(o), val_(o).f)
// 表类型
#define hvalue(o)	check_exp(ttistable(o), gco2t(val_(o).gc))
#define bvalue(o)	check_exp(ttisboolean(o), val_(o).b)
#define thvalue(o)	check_exp(ttisthread(o), gco2th(val_(o).gc))
/* a dead value may get the 'gc' field, but cannot access its contents */
#define deadvalue(o)	check_exp(ttisdeadkey(o), cast(void *, val_(o).gc))

#define l_isfalse(o)	(ttisnil(o) || (ttisboolean(o) && bvalue(o) == 0))


#define iscollectable(o)	(rttype(o) & BIT_ISCOLLECTABLE)


/* Macros for internal tests */
// 检测type tag是否正确
// 将自己的type tag 与 从gc类型中取到的type tag进行比对
#define righttt(obj)		(ttype(obj) == gcvalue(obj)->tt)

// 为真的条件
// 1.不是垃圾回收类型
// 2.isdead为否
#define checkliveness(L,obj) \
	lua_longassert(!iscollectable(obj) || \
		(righttt(obj) && (L == NULL || !isdead(G(L),gcvalue(obj)))))


/* Macros to set values */
// 一系列宏 用于赋值
// 主要操作是修改真实值以及修改type tag
#define settt_(o,t)	((o)->tt_=(t))

#define setfltvalue(obj,x) \
  { TValue *io=(obj); val_(io).n=(x); settt_(io, LUA_TNUMFLT); }

#define chgfltvalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisfloat(io)); val_(io).n=(x); }

#define setivalue(obj,x) \
  { TValue *io=(obj); val_(io).i=(x); settt_(io, LUA_TNUMINT); }

#define chgivalue(obj,x) \
  { TValue *io=(obj); lua_assert(ttisinteger(io)); val_(io).i=(x); }

#define setnilvalue(obj) settt_(obj, LUA_TNIL)

#define setfvalue(obj,x) \
  { TValue *io=(obj); val_(io).f=(x); settt_(io, LUA_TLCF); }

#define setpvalue(obj,x) \
  { TValue *io=(obj); val_(io).p=(x); settt_(io, LUA_TLIGHTUSERDATA); }

#define setbvalue(obj,x) \
  { TValue *io=(obj); val_(io).b=(x); settt_(io, LUA_TBOOLEAN); }

#define setgcovalue(L,obj,x) \
  { TValue *io = (obj); GCObject *i_g=(x); \
    val_(io).gc = i_g; settt_(io, ctb(i_g->tt)); }

// obj2gco只是进行了一次强制类型转换 将各种可回收类型指针 转换成 GCObject*类型
#define setsvalue(L,obj,x) \
  { TValue *io = (obj); TString *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(x_->tt)); \
    checkliveness(L,io); }

#define setuvalue(L,obj,x) \
  { TValue *io = (obj); Udata *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TUSERDATA)); \
    checkliveness(L,io); }

#define setthvalue(L,obj,x) \
  { TValue *io = (obj); lua_State *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTHREAD)); \
    checkliveness(L,io); }

#define setclLvalue(L,obj,x) \
  { TValue *io = (obj); LClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TLCL)); \
    checkliveness(L,io); }

#define setclCvalue(L,obj,x) \
  { TValue *io = (obj); CClosure *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TCCL)); \
    checkliveness(L,io); }

#define sethvalue(L,obj,x) \
  { TValue *io = (obj); Table *x_ = (x); \
    val_(io).gc = obj2gco(x_); settt_(io, ctb(LUA_TTABLE)); \
    checkliveness(L,io); }

#define setdeadvalue(obj)	settt_(obj, LUA_TDEADKEY)


// 直接进行赋值
#define setobj(L,obj1,obj2) \
	{ TValue *io1=(obj1); *io1 = *(obj2); \
	  (void)L; checkliveness(L,io1); }


/*
** different types of assignments, according to destination
*/

// 以下宏只是对上面各种赋值宏的重命名 更好的语义化
// 功能也是进行值复制
/* from stack to (same) stack */
#define setobjs2s	setobj
/* to stack (not from same stack) */
#define setobj2s	setobj
#define setsvalue2s	setsvalue
#define sethvalue2s	sethvalue
#define setptvalue2s	setptvalue
/* from table to same table */
#define setobjt2t	setobj
/* to new object */
#define setobj2n	setobj
#define setsvalue2n	setsvalue

/* to table (define it as an expression to be used in macros) */
#define setobj2t(L,o1,o2)  ((void)L, *(o1)=*(o2), checkliveness(L,(o1)))




/*
** {======================================================
** types and prototypes
** =======================================================
*/


typedef TValue *StkId;  /* index to stack elements */




/*
** Header for string value; string bytes follow the end of this structure
** (aligned according to 'UTString'; see next).
*/
typedef struct TString {
  CommonHeader;
  // 对于短字符串 用于标记是内部保留字,也就是关键字
  // 是不支持自动回收的,在GC过程中会略过对这个字符串的处理 luaX_tokens
  // 对于长字符串 用于标记已经计算了哈希值
  lu_byte extra;  /* reserved words for short strings; "has hash" for longs */
  // 短字符串的长度
  lu_byte shrlen;  /* length for short strings */
  // 字符串的hash值,字符串的比较可以通过hash值
  unsigned int hash;
  // 对于长字符串这里记录字符串长度
  // 对于短字符串这里记录下一个的地址
  union {
    size_t lnglen;  /* length for long strings */
    struct TString *hnext;  /* linked list for hash table */
  } u;
} TString;


/*
** Ensures that address after this type is always fully aligned.
*/
// 只是用来计算sizeof(UTString)代表TString的大小 没有其他作用
typedef union UTString {
  L_Umaxalign dummy;  /* ensures maximum alignment for strings */
  TString tsv;
} UTString;


/*
** Get the actual string (array of bytes) from a 'TString'.
** (Access to 'extra' ensures that value is really a 'TString'.)
*/
// 真实的字符串就存储在TString后面, 加上UTString的大小即可
#define getstr(ts)  \
  check_exp(sizeof((ts)->extra), cast(char *, (ts)) + sizeof(UTString))


/* get the actual string (array of bytes) from a Lua value */
// 从TValue类型取出真实字符串
#define svalue(o)       getstr(tsvalue(o))

/* get string length from 'TString *s' */
// 从TString获取字符串长度
#define tsslen(s)	((s)->tt == LUA_TSHRSTR ? (s)->shrlen : (s)->u.lnglen)

/* get string length from 'TValue *o' */
// 从TValue获取字符串长度
#define vslen(o)	tsslen(tsvalue(o))


/*
** Header for userdata; memory area follows the end of this structure
** (aligned according to 'UUdata'; see next).
*/
typedef struct Udata {
  // commonHeader中的tt 指定了类型位Udata
  CommonHeader;
  // ttuv_ 指定Udata内部存储的用户类型的类型
  lu_byte ttuv_;  /* user value's tag */
  struct Table *metatable;
  size_t len;  /* number of bytes */
  // ? 为什么不使用TValue类型 而是使用Value类型并且将类型放在外部使用ttuv_指定
  union Value user_;  /* user value */
} Udata;


/*
** Ensures that address after this type is always fully aligned.
*/
typedef union UUdata {
  L_Umaxalign dummy;  /* ensures maximum alignment for 'local' udata */
  Udata uv;
} UUdata;


/*
**  Get the address of memory block inside 'Udata'.
** (Access to 'ttuv_' ensures that value is really a 'Udata'.)
*/
#define getudatamem(u)  \
  check_exp(sizeof((u)->ttuv_), (cast(char*, (u)) + sizeof(UUdata)))

// 不论是get还是set都是两步 1.设置值 2.设置type tag
// 把o的值设置到u上 o是TValue,u是Udata
#define setuservalue(L,u,o) \
	{ const TValue *io=(o); Udata *iu = (u); \
	  iu->user_ = io->value_; iu->ttuv_ = rttype(io); \
	  checkliveness(L,io); }

// 把u的值写到o上 o是TValue,u是Udata
#define getuservalue(L,u,o) \
	{ TValue *io=(o); const Udata *iu = (u); \
	  io->value_ = iu->user_; settt_(io, iu->ttuv_); \
	  checkliveness(L,io); }


/*
** Description of an upvalue for function prototypes
*/
// upvalue的描述信息
typedef struct Upvaldesc {
  // upvalue的名称
  TString *name;  /* upvalue name (for debug information) */
  // 该upvalue是否在父函数栈上
  lu_byte instack;  /* whether it is in stack (register) */
  // 索引位置信息
  // 对于关闭的upvalue ,已经无法从栈上获取到,idx 指外层函数的upvalue 表中的索引号
  // 对于在数据栈上的 upvalue ,序号即是变量对应的寄存器号
  lu_byte idx;  /* index of upvalue (in stack or in outer function's list) */
} Upvaldesc;


/*
** Description of a local variable for function prototypes
** (used for debug information)
*/
// 局部变量的描述信息
typedef struct LocVar {
  TString *varname; // 变量名称
  // start和end指定了变量的作用域
  int startpc;  /* first point where variable is active 变量作用域开始位置 */
  int endpc;    /* first point where variable is dead   变量作用域结束位置 */
} LocVar;


/*
** Function Prototypes
*/

/*
typedef struct Proto{
  CommonHeader;
  
  // 1 函数原型信息
  lu_byte numparams;      // 函数参数个数
  lu_byte is_vararg;      // 是否有变长参数 1说明最后一个参数是可变参数
  lu_byte maxstacksize;   // 最大的函数栈长度 也就是最多占用的数据栈个数

  // 2 常量
  int sizek;              // 常量的个数
  TValue* k;              // 常量表
  
  // 3 局部变量
  int sizelocvars;        // 局部变量的个数
  LocVar* locvars;        // 局部变量数组

  // 4 闭包变量
  int sizeupvalues;       // 闭包变量的个数
  Upvaldesc* upvalues;    // 闭包变量数组

  // 5 子函数(嵌套)的Propo
  int sizep;              // 子函数的Propo数组长度
  struct Proto** p;       // 子函数的Propo数组
  struct LClosure* cache; // 缓存子函数的Propo数组

  // 6 该函数的指令
  int sizecode;           // 指令的个数(指令数组的长度)
  Instruction* code;      // 指令数组

  // 7 行信息 用于调试
  int sizelineinfo;       // 行信息数组长度
  int* lineinfo;          // 行信息数组

  // 8 源码信息
  int linedefined;        // 函数起始行
  int lastlinedefined;    // 函数结束行
  TString* source;        // 函数源码

  // 9 垃圾回收使用
  GCObject* gclist;
}Proto;
 */

// 函数原型
typedef struct Proto {
  CommonHeader;
  lu_byte numparams;  /* number of fixed parameters */
  lu_byte is_vararg;
  lu_byte maxstacksize;  /* number of registers needed by this function */
  int sizeupvalues;  /* size of 'upvalues' */
  int sizek;  /* size of 'k' */
  int sizecode;
  int sizelineinfo;
  int sizep;  /* size of 'p' */
  int sizelocvars;
  int linedefined;  /* debug information  */
  int lastlinedefined;  /* debug information  */
  TValue *k;  /* constants used by the function */
  Instruction *code;  /* opcodes */
  struct Proto **p;  /* functions defined inside the function */
  int *lineinfo;  /* map from opcodes to source lines (debug information) */
  LocVar *locvars;  /* information about local variables (debug information) */
  Upvaldesc *upvalues;  /* upvalue information */
  struct LClosure *cache;  /* last-created closure with this prototype */
  TString  *source;  /* used for debug information */
  GCObject *gclist;
} Proto;



/*
** Lua Upvalues
*/
typedef struct UpVal UpVal;


/*
** Closures
*/
// 闭包都属于LUA_TFUNCTION类型

// nupvalues指定upvalue的个数
#define ClosureHeader \
	CommonHeader; lu_byte nupvalues; GCObject *gclist

// luaF_newCclosure
// c函数闭包 包括一个c函数指针和相关的upvalue
typedef struct CClosure {
  ClosureHeader;
  lua_CFunction f;
  TValue upvalue[1];  /* list of upvalues */
} CClosure;


// luaF_newLclosure
// lua闭包 包括一个lua函数原型和相关的upvalue
typedef struct LClosure {
  ClosureHeader;
  struct Proto *p;
  UpVal *upvals[1];  /* list of upvalues */
} LClosure;


// 统一两种闭包类型
typedef union Closure {
  CClosure c;
  LClosure l;
} Closure;


// 检查是否是lua闭包
#define isLfunction(o)	ttisLclosure(o)

// 获取函数原型 就是获取lua闭包中的proto
#define getproto(o)	(clLvalue(o)->p)


/*
** Tables
*/

/*
TKey完全可以按照这种方式来定义
typedef struct TKey {
  TValue tvk;
  int next;
} TKey;
TValue内有Value占用8字节,int型tt_占用4字节 字节对齐浪费4字节总共16字节
加上int型next占用4字节按8字节对齐 总共24字节

但是如果按照下面的方式进行定义, TValuefields就占用了TValue的前12字节, next利用了字节对齐的4字节
总共只有16字节 每一个key都省了8个字节

这种定义在内存分布上等价于
typedef struct TKey {
  TValuefields;
  int next;
} TKey;
使用union可以转换为TValue类型,与之前的类型统一

*/
typedef union TKey {
  struct {
    TValuefields;
    int next;  /* for chaining (offset for next node) */
  } nk;
  TValue tvk;
} TKey;


/* copy a value into a key without messing up field 'next' */
// 向TKey类型写入一个TValue
// 直接写入tvk会导致next被覆盖
#define setnodekey(L,key,obj) \
	{ TKey *k_=(key); const TValue *io_=(obj); \
	  k_->nk.value_ = io_->value_; k_->nk.tt_ = io_->tt_; \
	  (void)L; checkliveness(L,io_); }


// Node就是table的一项 分为键和值
typedef struct Node {
  TValue i_val;
  TKey i_key;
} Node;

  // 结构图如下
  // https://img-blog.csdn.net/20170818102937416?watermark/2/text/aHR0cDovL2Jsb2cuY3Nkbi5uZXQvbGl1dGlhbnNoeDIwMTI=/font/5a6L5L2T/fontsize/400/fill/I0JBQkFCMA==/dissolve/70/gravity/SouthEast

typedef struct Table {
  CommonHeader;
  // flags 字段是一个 byte 类型，用于表示在这个表中提供了哪些元方法,默认为0；
  // 当查找过至少一次以后，如果该表中存在某个元方法，那么就将该元方法对应的 flag 位置为1,这样下一次查找时只需要判断该位即可
  // 所有的元方法映射的bit在 TMS 中
  lu_byte flags;  /* 1<<p means tagmethod(p) is not present */
  // lsizenode 字段是该表Hash桶大小的log2值,Hash桶数组大小一定是2的次方,当扩展Hash桶的时候每次需要乘以2。
  lu_byte lsizenode;  /* log2 of size of 'node' array */
  // sizearray 字段表示该表数组部分的size
  unsigned int sizearray;  /* size of 'array' array */
  // array 指向该表的数组部分的起始位置。
  TValue *array;  /* array part */
  // node 指向该表的Hash部分的起始位置。
  Node *node;
  // lastfree 指向Lua表的Hash 部分的末尾位置。
  Node *lastfree;  /* any free position is before this position */
  // metatable 元表。
  struct Table *metatable;
  // gclist GC相关的链表。 
  GCObject *gclist;
} Table;



/*
** 'module' operation for hashing (size is always a power of 2)
*/
// 求s%size 由于size一定是2的指数 所以使用了下面的位运算加速
// 当b是2的指数时 mod(a,b) 与 a&(b-1)等价 一般的编译器也会对这种情况进行优化
#define lmod(s,size) \
	(check_exp((size&(size-1))==0, (cast(int, (s) & ((size)-1)))))

// 2的x次方
#define twoto(x)	(1<<(x))
// 哈希表部分桶的大小
#define sizenode(t)	(twoto((t)->lsizenode))


/*
** (address of) a fixed nil value
*/
// 全局唯一的nil变量
#define luaO_nilobject		(&luaO_nilobject_)


LUAI_DDEC const TValue luaO_nilobject_;

/* size of buffer for 'luaO_utf8esc' function */
// 设置luaO_utf8esc函数缓存区的大小
#define UTF8BUFFSZ	8

LUAI_FUNC int luaO_int2fb (unsigned int x);
LUAI_FUNC int luaO_fb2int (int x);
LUAI_FUNC int luaO_utf8esc (char *buff, unsigned long x);
LUAI_FUNC int luaO_ceillog2 (unsigned int x);
LUAI_FUNC void luaO_arith (lua_State *L, int op, const TValue *p1,
                           const TValue *p2, TValue *res);
LUAI_FUNC size_t luaO_str2num (const char *s, TValue *o);
LUAI_FUNC int luaO_hexavalue (int c);
LUAI_FUNC void luaO_tostring (lua_State *L, StkId obj);
LUAI_FUNC const char *luaO_pushvfstring (lua_State *L, const char *fmt,
                                                       va_list argp);
LUAI_FUNC const char *luaO_pushfstring (lua_State *L, const char *fmt, ...);
LUAI_FUNC void luaO_chunkid (char *out, const char *source, size_t len);


#endif

