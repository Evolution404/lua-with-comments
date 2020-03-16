/*
** $Id: lopcodes.h,v 1.149 2016/07/19 17:12:21 roberto Exp $
** Opcodes for Lua virtual machine
** See Copyright Notice in lua.h
*/
// Lua虚拟机的操作码

#ifndef lopcodes_h
#define lopcodes_h

#include "llimits.h"


/*===========================================================================
  We assume that instructions are unsigned numbers.
  All instructions have an opcode in the first 6 bits.
  Instructions can have the following fields:
  所有指令最后6位都是用于表示操作码 剩余位数表示操作数 下面写出了各个操作数占用的位数
	'A' : 8 bits
	'B' : 9 bits
	'C' : 9 bits
	'Ax' : 26 bits ('A', 'B', and 'C' together)
	'Bx' : 18 bits ('B' and 'C' together)
	'sBx' : signed Bx
  sBx有18位 为表示有符号数使用0表示最小负数 全1表示最大正数
  表示实际结果是 表示数-2^17 即表示0是2^17
  A signed argument is represented in excess K; that is, the number
  value is the unsigned value minus K. K is exactly the maximum value
  for that argument (so that -max is represented by 0, and +max is
  represented by 2*max), which is half the maximum for the corresponding
  unsigned argument.
===========================================================================*/


enum OpMode {iABC, iABx, iAsBx, iAx};  /* basic instruction format */
/*
                                      各个指令的结构
 +------------------------------------------------------------------------------------------------------+
 |      |31|30|29|28|27|26|25|24|23|22|21|20|19|18|17|16|15|14|13|12|11|10| 9| 8| 7| 6| 5| 4| 3| 2| 1| 0|
 +------+-----------------------+-----------------------------+-----------------------+-----------------+
 |iABC  |         B:9           |            C:9              |         A:8           |   opcode:6      |
 +------+-----------------------+-----------------------------+-----------------------+-----------------+
 |iABx  |                      Bx:18                          |         A:8           |   opcode:6      |
 +------+-----------------------------------------------------+-----------------------+-----------------+
 |iAsBx |                      sBx:18                         |         A:8           |   opcode:6      |
 +------+-----------------------------------------------------+-----------------------+-----------------+
 |iAx   |                      Ax:26                                                  |   opcode:6      |
 +------------------------------------------------------------------------------------+-----------------+
*/


/*
** size and position of opcode arguments.
*/
#define SIZE_C		9
#define SIZE_B		9
#define SIZE_Bx 	(SIZE_C + SIZE_B)          // 18
#define SIZE_A		8
#define SIZE_Ax		(SIZE_C + SIZE_B + SIZE_A) // 26

#define SIZE_OP		6                          // 所有操作码的长度都是6位

#define POS_OP		0
#define POS_A		(POS_OP + SIZE_OP)           // 6 上图A的第一位也是6
#define POS_C		(POS_A + SIZE_A)             // 14
#define POS_B		(POS_C + SIZE_C)             // 23
#define POS_Bx		POS_C                      // 14
#define POS_Ax		POS_A                      // 6


/*
** limits for opcode arguments.
** we use (signed) int to manipulate most arguments,
** so they must fit in LUAI_BITSINT-1 bits (-1 for sign)
*/
#if SIZE_Bx < LUAI_BITSINT-1
#define MAXARG_Bx        ((1<<SIZE_Bx)-1) // 2^18-1
#define MAXARG_sBx        (MAXARG_Bx>>1)  // 2^17-1      /* 'sBx' is signed */
#else
#define MAXARG_Bx        MAX_INT
#define MAXARG_sBx        MAX_INT
#endif

#if SIZE_Ax < LUAI_BITSINT-1
#define MAXARG_Ax	((1<<SIZE_Ax)-1)        // 2^6-1
#else
#define MAXARG_Ax	MAX_INT
#endif


#define MAXARG_A        ((1<<SIZE_A)-1)   // 2^8-1
#define MAXARG_B        ((1<<SIZE_B)-1)   // 2^9-1
#define MAXARG_C        ((1<<SIZE_C)-1)   // 2^9-1


/* creates a mask with 'n' 1 bits at position 'p' */
// ~0 32位1
// ~0<<1 最后一位变成0
// ~(~0<<1) 全0最后一位为1
// (~(~0<<1))<<1 第1位为1 其他位是0 第n位代表从后往前数 且从0开始
// 这个宏的意义就是在第p位标记n个1 其余位是0
// 例如MASK1(2,3) 结果是11000
#define MASK1(n,p)	((~((~(Instruction)0)<<(n)))<<(p))

/* creates a mask with 'n' 0 bits at position 'p' */
// 这个宏的意义就是在第p位标记n个0 其余位是1
// 其实就是上面的宏取非
#define MASK0(n,p)	(~MASK1(n,p))

/*
** the following macros help to manipulate instructions
*/
// 用于操作指令的宏

// get宏原理都是先将指令移位将要操作的值移动到最后再利用MASK1生成的结果进行与运算得到需要的值
// set宏原理 将需要设置值的位置清空 处理需要设置值为对应值其余位为0 将两者进行或运算

#define GET_OPCODE(i)	(cast(OpCode, ((i)>>POS_OP) & MASK1(SIZE_OP,0))) // 得到操作码
#define SET_OPCODE(i,o)	((i) = (((i)&MASK0(SIZE_OP,POS_OP)) | \
		((cast(Instruction, o)<<POS_OP)&MASK1(SIZE_OP,POS_OP))))           // 设置操作码

#define getarg(i,pos,size)	(cast(int, ((i)>>pos) & MASK1(size,0)))
#define setarg(i,v,pos,size)	((i) = (((i)&MASK0(size,pos)) | \
                ((cast(Instruction, v)<<pos)&MASK1(size,pos))))

#define GETARG_A(i)	getarg(i, POS_A, SIZE_A)
#define SETARG_A(i,v)	setarg(i, v, POS_A, SIZE_A)

#define GETARG_B(i)	getarg(i, POS_B, SIZE_B)
#define SETARG_B(i,v)	setarg(i, v, POS_B, SIZE_B)

#define GETARG_C(i)	getarg(i, POS_C, SIZE_C)
#define SETARG_C(i,v)	setarg(i, v, POS_C, SIZE_C)

#define GETARG_Bx(i)	getarg(i, POS_Bx, SIZE_Bx)
#define SETARG_Bx(i,v)	setarg(i, v, POS_Bx, SIZE_Bx)

#define GETARG_Ax(i)	getarg(i, POS_Ax, SIZE_Ax)
#define SETARG_Ax(i,v)	setarg(i, v, POS_Ax, SIZE_Ax)

// sBx的表示范围 -(2^17-1) 到 2^17-1 是对称分布 有一个值没有利用
#define GETARG_sBx(i)	(GETARG_Bx(i)-MAXARG_sBx)  // GETARG_Bx最小是0 所以sBx最小是-(2^17-1)
#define SETARG_sBx(i,b)	SETARG_Bx((i),cast(unsigned int, (b)+MAXARG_sBx))


#define CREATE_ABC(o,a,b,c)	((cast(Instruction, o)<<POS_OP) \
			| (cast(Instruction, a)<<POS_A) \
			| (cast(Instruction, b)<<POS_B) \
			| (cast(Instruction, c)<<POS_C))

#define CREATE_ABx(o,a,bc)	((cast(Instruction, o)<<POS_OP) \
			| (cast(Instruction, a)<<POS_A) \
			| (cast(Instruction, bc)<<POS_Bx))

#define CREATE_Ax(o,a)		((cast(Instruction, o)<<POS_OP) \
			| (cast(Instruction, a)<<POS_Ax))


/*
** Macros to operate RK indices
*/
// 常量的位置用9位标记 倒数第9位(第8位)是1说明是常量 最后8位说明位置

/* this bit 1 means constant (0 means register) */
// 一个值第8位是1说明是常量 是0说明是寄存器值
#define BITRK		(1 << (SIZE_B - 1))  // 100000000 第8位是1

/* test whether value is a constant */
// 第8位是1说明是常量
#define ISK(x)		((x) & BITRK)

/* gets the index of the constant */
// 去掉第8位的1 取最后面8位就是该常量在常量表的下标
#define INDEXK(r)	((int)(r) & ~BITRK)

#if !defined(MAXINDEXRK)  /* (for debugging only) */
#define MAXINDEXRK	(BITRK - 1)
#endif

/* code a constant index as a RK value */
#define RKASK(x)	((x) | BITRK)


/*
** invalid register that fits in 8 bits
*/
#define NO_REG		MAXARG_A


/*
** R(x) - register                                  一定是寄存器索引 一定要访问Lua栈
** Kst(x) - constant (in constant table)            一定是常量 在常量表中
** RK(x) == if ISK(x) then Kst(INDEXK(x)) else R(x) 可能是常量也可能在Lua栈中
*/


/*
** grep "ORDER OP" if you change these enums
*/

typedef enum {
/*----------------------------------------------------------------------
name		args	description
------------------------------------------------------------------------*/
OP_MOVE,/*	A B	R(A) := R(B)					*/
OP_LOADK,/*	A Bx	R(A) := Kst(Bx)					*/
OP_LOADKX,/*	A 	R(A) := Kst(extra arg)				*/
OP_LOADBOOL,/*	A B C	R(A) := (Bool)B; if (C) pc++			*/
// 参数B+1是设置nil的个数
OP_LOADNIL,/*	A B	R(A), R(A+1), ..., R(A+B) := nil		*/
OP_GETUPVAL,/*	A B	R(A) := UpValue[B]				*/

OP_GETTABUP,/*	A B C	R(A) := UpValue[B][RK(C)]			*/
OP_GETTABLE,/*	A B C	R(A) := R(B)[RK(C)]				*/

OP_SETTABUP,/*	A B C	UpValue[A][RK(B)] := RK(C)			*/
OP_SETUPVAL,/*	A B	UpValue[B] := R(A)				*/
OP_SETTABLE,/*	A B C	R(A)[RK(B)] := RK(C)				*/

// 在R(A)位置新建一个表 参数B代表array部分大小 参数C代表hash部分大小
// 参数BC的表示方法查看luaO_int2fb
OP_NEWTABLE,/*	A B C	R(A) := {} (size = B,C)				*/

// 例如语句a:b('x')
// 开始时参数B存放表a,参数C存放字符串常量'b'
// 执行后R(A)存放a.b函数,R(A+1)存放表a代表self变量
OP_SELF,/*	A B C	R(A+1) := R(B); R(A) := R(B)[RK(C)]		*/

OP_ADD,/*	A B C	R(A) := RK(B) + RK(C)				*/
OP_SUB,/*	A B C	R(A) := RK(B) - RK(C)				*/
OP_MUL,/*	A B C	R(A) := RK(B) * RK(C)				*/
OP_MOD,/*	A B C	R(A) := RK(B) % RK(C)				*/
OP_POW,/*	A B C	R(A) := RK(B) ^ RK(C)				*/
OP_DIV,/*	A B C	R(A) := RK(B) / RK(C)				*/
OP_IDIV,/*	A B C	R(A) := RK(B) // RK(C)				*/
OP_BAND,/*	A B C	R(A) := RK(B) & RK(C)				*/
OP_BOR,/*	A B C	R(A) := RK(B) | RK(C)				*/
OP_BXOR,/*	A B C	R(A) := RK(B) ~ RK(C)				*/
OP_SHL,/*	A B C	R(A) := RK(B) << RK(C)				*/
OP_SHR,/*	A B C	R(A) := RK(B) >> RK(C)				*/
OP_UNM,/*	A B	R(A) := -R(B)					*/
OP_BNOT,/*	A B	R(A) := ~R(B)					*/
OP_NOT,/*	A B	R(A) := not R(B)				*/
OP_LEN,/*	A B	R(A) := length of R(B)				*/

OP_CONCAT,/*	A B C	R(A) := R(B).. ... ..R(C)			*/

OP_JMP,/*	A sBx	pc+=sBx; if (A) close all upvalues >= R(A - 1)	*/
// OP_EQ,OP_LT,OP_LE都会紧跟一条OP_JMP指令
// 注意是~=A,也就是(RK(B)==RK(C))==A会执行下一句的跳转语句
OP_EQ,/*	A B C	if ((RK(B) == RK(C)) ~= A) then pc++		*/
OP_LT,/*	A B C	if ((RK(B) <  RK(C)) ~= A) then pc++		*/
OP_LE,/*	A B C	if ((RK(B) <= RK(C)) ~= A) then pc++		*/

OP_TEST,/*	A C	if not (R(A) <=> C) then pc++			*/
OP_TESTSET,/*	A B C	if (R(B) <=> C) then R(A) := R(B) else pc++	*/

// R(A)存放被调用的函数 B=1说明没有参数 B=2或更大说明有B-1个参数且R(A+1)是base
// B=0代表top, R(A+1)到R(top-1)都是参数 这种可能出现在前一条指令是OP_CALL或者OP_VARARG 因为参数个数不确定
// C=1说明不保存返回值 C=2或更大保存C-1个返回值
// C=0代表外部接收返回值个数不确定 例如a(b()) 先调用b函数 b函数的返回接收个数就不确定
OP_CALL,/*	A B C	R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1)) */
OP_TAILCALL,/*	A B C	return R(A)(R(A+1), ... ,R(A+B-1))		*/
// 返回值个数是B-1,也就是B=1说明没有返回值
// B=0说明上一条指令是OP_CALL或OP_VARARG,返回值就是从R(A)到R(top-1)
OP_RETURN,/*	A B	return R(A), ... ,R(A+B-2)	(see note)	*/

// 这是for循环的结束指令,将index+step,与limit比较,如果小于等于limit,对循环变量进行赋值,再跳转到OP_FORPREP下一条指令
OP_FORLOOP,/*	A sBx	R(A)+=R(A+2);
			if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }*/
// 这是for循环的开始指令,将index-step然后跳转到OP_FORLOOP
OP_FORPREP,/*	A sBx	R(A)-=R(A+2); pc+=sBx				*/

// 第二种for循环首先生成排放好三个变量generator,state,control
// 前三个寄存器分别是generator,state,control,后两个是i,v,总共占用五个寄存器
// 该指令调用generator处函数并传入state和control,返回值设置到i和v
OP_TFORCALL,/*	A C	R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2));	*/
// 该指令判断i是否为空,不为空设置control为i并跳转回循环体
OP_TFORLOOP,/*	A sBx	if R(A+1) ~= nil then { R(A)=R(A+1); pc += sBx }*/

// FPF是'fields per flush' 由宏LFIELDS_PER_FLUSH定义 默认是50
// 该指令用于设置R(A)位置的表的数组部分,用来初始化的值放置在R(A+1)...R(A+B)上
// 参数B是设置的个数 参数C是设置这个表的第几块
//   设置表是按照块来的,每个块是FPF个也就是50,如果不分块一次可能占用上万个寄存器
//   如果指令是 0 10 1,也就是让0位置的表的1,2,3...10初始化
//   如果指令是 0 20 4,也就是让0位置的表的151,152,153...170初始化
// 参数B为0 代表个数不确定直接一直使用到栈顶进行初始化
// 参数C为0 代表参数C存在下一个指令中,因为这个指令可能存不开C
OP_SETLIST,/*	A B C	R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B	*/

// Bx是指在外部函数的子函数表的下标
OP_CLOSURE,/*	A Bx	R(A) := closure(KPROTO[Bx])			*/

// 参数B=0,代表拷贝尽可能多的值
// 参数B不为0,代表拷贝B-1个值
OP_VARARG,/*	A B	R(A), R(A+1), ..., R(A+B-2) = vararg		*/

OP_EXTRAARG/*	Ax	extra (larger) argument for previous opcode	*/
} OpCode;


#define NUM_OPCODES	(cast(int, OP_EXTRAARG) + 1)  // 操作码的个数



/*===========================================================================
  Notes:
  (*) In OP_CALL, if (B == 0) then B = top. If (C == 0), then 'top' is
  set to last_result+1, so next open instruction (OP_CALL, OP_RETURN,
  OP_SETLIST) may use 'top'.

  (*) In OP_VARARG, if (B == 0) then use actual number of varargs and
  set top (like in OP_CALL with C == 0).

  (*) In OP_RETURN, if (B == 0) then return up to 'top'.

  (*) In OP_SETLIST, if (B == 0) then B = 'top'; if (C == 0) then next
  'instruction' is EXTRAARG(real C).

  (*) In OP_LOADKX, the next 'instruction' is always EXTRAARG.
  OP_LOADKX的下一个指令一定是OP_EXTRAARG

  (*) For comparisons, A specifies what condition the test should accept
  (true or false).

  (*) All 'skips' (pc++) assume that next instruction is a jump.
  所有使用pc++的地方都假设下一条指令是跳转指令 即OP_JMP

===========================================================================*/


/*
** masks for instruction properties. The format is:
** bits 0-1: op mode
** bits 2-3: C arg mode
** bits 4-5: B arg mode
** bit 6: instruction set register A
** bit 7: operator is a test (next instruction must be a jump)
*/

enum OpArgMask {
  OpArgN,  /* argument is not used */
  OpArgU,  /* argument is used */
  OpArgR,  /* argument is a register or a jump offset */
  OpArgK   /* argument is a constant or register/constant */
};

LUAI_DDEC const lu_byte luaP_opmodes[NUM_OPCODES];
// luaP_opmodes标记了各个指令的信息
// 0-1位 指令的类型 iABC iABx iAsBx iAx
// 2-3位 参数C的类型
// 4-5位 参数B的类型
// 6位   指令是否修改了寄存器A
// 7位   指令是否是测试指令 下一条指令一定是jump指令

#define getOpMode(m)	(cast(enum OpMode, luaP_opmodes[m] & 3))         // 获取指令的类型
#define getBMode(m)	(cast(enum OpArgMask, (luaP_opmodes[m] >> 4) & 3)) // 获取参数B的类型
#define getCMode(m)	(cast(enum OpArgMask, (luaP_opmodes[m] >> 2) & 3)) // 获取参数C的类型
#define testAMode(m)	(luaP_opmodes[m] & (1 << 6))                     // 检查指令是否修改A寄存器
// 测试指令包括 OP_EQ,OP_LT,OP_LE,OP_TEST,OP_TESTSET
#define testTMode(m)	(luaP_opmodes[m] & (1 << 7))                     // 检查指令是否是测试指令


// 记录了所有的指令的名称
LUAI_DDEC const char *const luaP_opnames[NUM_OPCODES+1];  /* opcode names */


/* number of list items to accumulate before a SETLIST instruction */
// SETLIST指令之前积累的list项的个数
#define LFIELDS_PER_FLUSH	50


#endif
