/*
** $Id: lmem.c,v 1.91 2015/03/06 19:45:54 roberto Exp $
** Interface to Memory Manager 内存管理接口
** See Copyright Notice in lua.h
*/

#define lmem_c
#define LUA_CORE

#include "lprefix.h"


#include <stddef.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"



/*
** About the realloc function:
** void * frealloc (void *ud, void *ptr, size_t osize, size_t nsize);
** ('osize' is the old size, 'nsize' is the new size)
**
** * frealloc(ud, NULL, x, s) creates a new block of size 's' (no
** matter 'x').
**
** * frealloc(ud, p, x, 0) frees the block 'p'
** (in this specific case, frealloc must return NULL);
** particularly, frealloc(ud, NULL, 0, 0) does nothing
** (which is equivalent to free(NULL) in ISO C)
**
** frealloc returns NULL if it cannot create or reallocate the area
** (any reallocation to an equal or smaller size cannot fail!)
*/

// 有关 realloc 函数
// void * frealloc (void *ud, void *ptr, size_t osize, size_t nsize);
// 'osize' 是 old size, 'nsize' 是 new size
// frealloc(ud, NULL, x, s) 分配一块s大小的内存(不管x)
// frealloc(ud, p, x, 0) 释放p指向的内存块 这种情况下返回值是NULL
//  特别的 frealloc(ud, NULL, 0, 0) 什么也不做 等价于 free(NULL)
// 如果创建或重新分配空间失败应该返回NULL, 重新分配的空间小于等于当前空间时永远不会失败



#define MINSIZEARRAY	4


// 用来管理可变长数组 主要策略是:当数组空间不够时,扩大为原来空间的两倍
// 内存管理部分使用了基于luaM_realloc_函数的宏luaM_reallocv,该宏针对数组操作,根据新的数组元素个数重新分配内存
// size当前数组大小 limit数组大小上限
void *luaM_growaux_ (lua_State *L, void *block, int *size, size_t size_elems,
                     int limit, const char *what) {
  void *newblock;
  int newsize;
  if (*size >= limit/2) {  /* cannot double it? 大小不能翻倍 */
    if (*size >= limit)  /* cannot grow even a little? size已经达到了最大值 */
      luaG_runerror(L, "too many %s (limit is %d)", what, limit);
    newsize = limit;  /* still have at least one free place 虽然不能翻倍但是也能扩张一些到limit */
  }
  else {
    newsize = (*size)*2;
    if (newsize < MINSIZEARRAY)  // 新空间最小是4
      newsize = MINSIZEARRAY;  /* minimum size */
  }
  // 重新分配空间
  newblock = luaM_reallocv(L, block, *size, newsize, size_elems);
  *size = newsize;  /* update only when everything else is OK 修改size */
  return newblock;
}


l_noret luaM_toobig (lua_State *L) {
  luaG_runerror(L, "memory allocation error: block too big");
}



/*
** generic allocation routine.
*/
// 通用内存分配函数
// block需要重新分配的内存 osize old size, nsize new size
// block 不为NULL, osize block的大小, nsize 新大小 对block重新分配
// block 不为NULL, osize block的大小, nsize 0  释放block
// block NULL, osize 任意值, nsize 新大小  分配一块nsize大小的空间并返回
// block NULL, osize 任意值, nsize 0  等价于free(NULL) 什么也不做
void *luaM_realloc_ (lua_State *L, void *block, size_t osize, size_t nsize) {
  void *newblock;
  global_State *g = G(L);
  size_t realosize = (block) ? osize : 0;  // block为NULL 说明osize是0
  lua_assert((realosize == 0) == (block == NULL));
#if defined(HARDMEMTESTS)
  if (nsize > realosize && g->gcrunning)
    luaC_fullgc(L, 1);  /* force a GC whenever possible */
#endif
  newblock = (*g->frealloc)(g->ud, block, osize, nsize);
  if (newblock == NULL && nsize > 0) { // 内存分配失败
    lua_assert(nsize > realosize);  /* cannot fail when shrinking a block 缩小空间时不应该会失败 */
    if (g->version) {  /* is state fully built? 尝试gc后再次进行分配内存 */
      luaC_fullgc(L, 1);  /* try to free some memory... */
      // g->frealloc 实际函数 l_alloc
      // nsize是0   调用free(block)
      // nsize不是0 调用realloc(block, nsize)
      newblock = (*g->frealloc)(g->ud, block, osize, nsize);  /* try again */
    }
    if (newblock == NULL)  // 重新分配还是失败 抛出错误
      luaD_throw(L, LUA_ERRMEM);
  }
  // nsize是0 newblock是NULL
  // nsize不是0 newblock一定不是NULL
  lua_assert((nsize == 0) == (newblock == NULL));
  g->GCdebt = (g->GCdebt + nsize) - realosize; // GCdebt 加上新增加的空间
  return newblock;
}

