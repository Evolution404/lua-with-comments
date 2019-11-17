/*
** $Id: lctype.h,v 1.12 2011/07/15 12:50:29 roberto Exp $
** 'ctype' functions for Lua
** See Copyright Notice in lua.h
*/

#ifndef lctype_h
#define lctype_h

#include "lua.h"


/*
** WARNING: the functions defined here do not necessarily correspond
** to the similar functions in the standard C ctype.h. They are
** optimized for the specific needs of Lua
*/

#if !defined(LUA_USE_CTYPE)

#if 'A' == 65 && '0' == 48
/* ASCII case: can use its own tables; faster and fixed */
// 不使用标准库中的ctype 使用自定义的函数更快
#define LUA_USE_CTYPE	0
#else
/* must use standard C ctype */
#define LUA_USE_CTYPE	1
#endif

#endif


#if !LUA_USE_CTYPE	/* { */

#include <limits.h>

#include "llimits.h"


// 在luai_ctype_中各个元素需要标记的位置
#define ALPHABIT	0  // 例如一个字符是字符 那么左移0位标记为1 也就是最后一位
#define DIGITBIT	1
#define PRINTBIT	2
#define SPACEBIT	3
#define XDIGITBIT	4


#define MASK(B)		(1 << (B))


/*
** add 1 to char to allow index -1 (EOZ)
*/
#define testprop(c,p)	(luai_ctype_[(c)+1] & (p))

/*
** 'lalpha' (Lua alphabetic) and 'lalnum' (Lua alphanumeric) both include '_'
*/
#define lislalpha(c)	testprop(c, MASK(ALPHABIT)) // A-Za-z以及_
#define lislalnum(c)	testprop(c, (MASK(ALPHABIT) | MASK(DIGITBIT)))
#define lisdigit(c)	testprop(c, MASK(DIGITBIT))
#define lisspace(c)	testprop(c, MASK(SPACEBIT))
#define lisprint(c)	testprop(c, MASK(PRINTBIT))
#define lisxdigit(c)	testprop(c, MASK(XDIGITBIT))

/*
** this 'ltolower' only works for alphabetic characters
*/
// 将大写字母倒数第6位标记为1就变成了小写
// 大小写相差32
#define ltolower(c)	((c) | ('A' ^ 'a'))


/* two more entries for 0 and -1 (EOZ) */

// maximum unsigned char value
// UCHAR_MAX 是 255
LUAI_DDEC const lu_byte luai_ctype_[UCHAR_MAX + 2];


#else			/* }{ */

/*
** use standard C ctypes
*/
// 使用c标准库
#include <ctype.h>


#define lislalpha(c)	(isalpha(c) || (c) == '_')  // 大小写字母或_
#define lislalnum(c)	(isalnum(c) || (c) == '_')  // 大小写字母 数字或_
#define lisdigit(c)	(isdigit(c))  // 数字
/*
 * 空白符
 * ' '	  0x20	空格 (SPC)
 * '\t'	0x09	水平制表符 (TAB)
 * '\n'	0x0a	换行符 (LF)
 * '\v'	0x0b	垂直制表符 (VT)
 * '\f'	0x0c	换页 (FF)
 * '\r'	0x0d	回车 (CR)
*/
#define lisspace(c)	(isspace(c))  // 是否空白符
#define lisprint(c)	(isprint(c))  // 是否可打印字符
#define lisxdigit(c)	(isxdigit(c))  // 是否16进制数字 0-9 和 A-F(或a-f)

#define ltolower(c)	(tolower(c))  // 转换成小写

#endif			/* } */

#endif

