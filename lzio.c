/*
** $Id: lzio.c,v 1.37 2015/09/08 15:41:05 roberto Exp $
** Buffered streams 缓冲流
** See Copyright Notice in lua.h
*/

#define lzio_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "llimits.h"
#include "lmem.h"
#include "lstate.h"
#include "lzio.h"

// 用于填充ZIO对象的缓存区 当z->n为0 如果要从ZIO中继续读取需要调用该函数
// 调用reader函数 ZIO修改对象中的n和size
int luaZ_fill (ZIO *z) {
  size_t size;
  lua_State *L = z->L;
  const char *buff;
  lua_unlock(L);
  buff = z->reader(L, z->data, &size); // getF 从z->data中读取内容 size为读取到的长度 buff为读取到的内容
  lua_lock(L);
  if (buff == NULL || size == 0) // 读取结束
    return EOZ;
  z->n = size - 1;  /* discount char being returned 使用reader读取了size个 最后返回了一个 所以还剩size-1 */
  z->p = buff;
  // 上面让p指向了buff 返回了一个值那么p要后移一个
  return cast_uchar(*(z->p++));  // *p++ 与 *(p++)一样 得到*p的值再对p自增
}


// 初始化一个ZIO对象
void luaZ_init (lua_State *L, ZIO *z, lua_Reader reader, void *data) {
  z->L = L;
  z->reader = reader;
  z->data = data;
  z->n = 0;
  z->p = NULL;
}


/* --------------------------------------------------------------- read --- */
// 从z中读取n个字符到b中
// 返回值是 n-读到的字节数
size_t luaZ_read (ZIO *z, void *b, size_t n) {
  while (n) {
    size_t m;
    if (z->n == 0) {  /* no bytes in buffer? 剩余可以直接读取的部分为0 */
      if (luaZ_fill(z) == EOZ)  /* try to read more 尝试填充缓存区 */
        return n;  /* no more input; return number of missing bytes 已经读取到底了直接返回n */
      else {
        // 填充成功了 luaZ_fill默认会返回一个字符 这里将默认消耗的字符放回去
        z->n++;  /* luaZ_fill consumed first byte; put it back */
        z->p--;
      }
    }
    // m就是这一轮读取的字节数 取n和z->n的最小值
    m = (n <= z->n) ? n : z->n;  /* min. between n and z->n */
    memcpy(b, z->p, m);
    z->n -= m;
    z->p += m;
    b = (char *)b + m;
    n -= m;
  }
  return 0;
}

