/*
** $Id: lmem.h,v 1.43 2014/12/19 17:26:14 roberto Exp $
** Interface to Memory Manager 内存管理接口
** See Copyright Notice in lua.h
*/

#ifndef lmem_h
#define lmem_h


#include <stddef.h>

#include "llimits.h"
#include "lua.h"


/*
** This macro reallocs a vector 'b' from 'on' to 'n' elements, where
** each element has size 'e'. In case of arithmetic overflow of the
** product 'n'*'e', it raises an error (calling 'luaM_toobig'). Because
** 'e' is always constant, it avoids the runtime division MAX_SIZET/(e).
**
** (The macro is somewhat complex to avoid warnings:  The 'sizeof'
** comparison avoids a runtime comparison when overflow cannot occur.
** The compiler should be able to optimize the real test by itself, but
** when it does it, it may give a warning about "comparison is always
** false due to limited range of data type"; the +1 tricks the compiler,
** avoiding this warning but also this optimization.)
*/
// 这个宏重新分配数组b的内存 从on个元素到n个元素 每个元素的大小是e
// (n+1) > MAX_SIZET/e 目的是确保n*e不会溢出
#define luaM_reallocv(L,b,on,n,e) \
  (((sizeof(n) >= sizeof(size_t) && cast(size_t, (n)) + 1 > MAX_SIZET/(e)) \
      ? luaM_toobig(L) : cast_void(0)) , \
   luaM_realloc_(L, (b), (on)*(e), (n)*(e)))

/*
** Arrays of chars do not need any test
*/
// 分配字符串数组
#define luaM_reallocvchar(L,b,on,n)  \
    cast(char *, luaM_realloc_(L, (b), (on)*sizeof(char), (n)*sizeof(char)))

// 释放内存 指定块的大小 不需要传nsize了 一定是0
#define luaM_freemem(L, b, s)	luaM_realloc_(L, (b), (s), 0)
// 释放内存 不指定块的大小 释放的大小就是块的结构体的大小
#define luaM_free(L, b)		luaM_realloc_(L, (b), sizeof(*(b)), 0)
// 释放数组的内存 传入指针和元素的个数
#define luaM_freearray(L, b, n)   luaM_realloc_(L, (b), (n)*sizeof(*(b)), 0)

// 分配s大小的内存
#define luaM_malloc(L,s)	luaM_realloc_(L, NULL, 0, (s))
// 分配t类型大小的内存 并进行类型转换
#define luaM_new(L,t)		cast(t *, luaM_malloc(L, sizeof(t)))
// 分配t类型n个元素数组
#define luaM_newvector(L,n,t) \
		cast(t *, luaM_reallocv(L, NULL, 0, n, sizeof(t)))

// 直接分配s大小内存 tag没有实际意义只是为了调用处的可读性
#define luaM_newobject(L,tag,s)	luaM_realloc_(L, NULL, tag, (s))

// 在数组已经放满的情况下增长数组的大小 一般是进行翻倍,调用后数组大小最小为4
// v要增长的数组 nelems数组中当前元素的个数 size数组当前的大小
// t数组元素类型 limit数组大小上限 e用于错误信息
#define luaM_growvector(L,v,nelems,size,t,limit,e) \
          if ((nelems)+1 > (size)) \
            ((v)=cast(t *, luaM_growaux_(L,v,&(size),sizeof(t),limit,e)))

// 重新分配数组大小并进行类型转换
// v 要重新分配的数组 oldn分配前元素个数 n要分配的元素个数 t元素的类型
#define luaM_reallocvector(L, v,oldn,n,t) \
   ((v)=cast(t *, luaM_reallocv(L, v, oldn, n, sizeof(t))))

LUAI_FUNC l_noret luaM_toobig (lua_State *L);

/* not to be called directly */
LUAI_FUNC void *luaM_realloc_ (lua_State *L, void *block, size_t oldsize,
                                                          size_t size);
LUAI_FUNC void *luaM_growaux_ (lua_State *L, void *block, int *size,
                               size_t size_elem, int limit,
                               const char *what);

#endif

