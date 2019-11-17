/*
** $Id: lzio.h,v 1.31 2015/09/08 15:41:05 roberto Exp $
** Buffered streams 缓冲流
** See Copyright Notice in lua.h
*/


#ifndef lzio_h
#define lzio_h

#include "lua.h"

#include "lmem.h"


#define EOZ	(-1)			/* end of stream */

typedef struct Zio ZIO;

// 从ZIO中读取一个字符
// 如果n>0说明缓存区还有值 直接返回 让p后移
// p=0 说明缓存区读完了 使用luaZ_fill填充缓存区
#define zgetc(z)  (((z)->n--)>0 ?  cast_uchar(*(z)->p++) : luaZ_fill(z))


typedef struct Mbuffer {
  char *buffer;    // 缓存的实际区域
  size_t n;        // 缓存元素的个数
  size_t buffsize; // 缓存区的长度
} Mbuffer;

// 初始化缓存
#define luaZ_initbuffer(L, buff) ((buff)->buffer = NULL, (buff)->buffsize = 0)

#define luaZ_buffer(buff)	((buff)->buffer)
#define luaZ_sizebuffer(buff)	((buff)->buffsize)
#define luaZ_bufflen(buff)	((buff)->n)

#define luaZ_buffremove(buff,i)	((buff)->n -= (i))
#define luaZ_resetbuffer(buff) ((buff)->n = 0)


// 重新分配buff的空间
#define luaZ_resizebuffer(L, buff, size) \
	((buff)->buffer = luaM_reallocvchar(L, (buff)->buffer, \
				(buff)->buffsize, size), \
	(buff)->buffsize = size)

// 释放buff占用的空间
#define luaZ_freebuffer(L, buff)	luaZ_resizebuffer(L, buff, 0)


LUAI_FUNC void luaZ_init (lua_State *L, ZIO *z, lua_Reader reader,
                                        void *data);
LUAI_FUNC size_t luaZ_read (ZIO* z, void *b, size_t n);	/* read next n bytes */



/* --------- Private Part ------------------ */

// n:p指向的区域可以读的字节数
// p:可以读的缓存区 n指示了p可以读的长度 n=0时p没有意义
// reader:通过reader读取值并修改为对应的n和p
// data:额外的数据

struct Zio {
  size_t n;			/* bytes still unread 未读的字节数*/
  const char *p;		/* current position in buffer 当前在buffer中的位置 */
  lua_Reader reader;		/* reader function 读取函数 getF */
  void *data;			/* additional data 额外的数据 */
  lua_State *L;			/* Lua state (for reader) 当前reader所在的线程 */
};


LUAI_FUNC int luaZ_fill (ZIO *z);

#endif
