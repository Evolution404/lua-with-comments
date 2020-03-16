/*
** $Id: lstring.c,v 2.56 2015/11/23 11:32:51 roberto Exp $
** String table (keeps all strings handled by Lua)
** See Copyright Notice in lua.h
*/

#define lstring_c
#define LUA_CORE

#include "lprefix.h"


#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lmem.h"
#include "lobject.h"
#include "lstate.h"
#include "lstring.h"


#define MEMERRMSG       "not enough memory"


/*
** Lua will use at most ~(2^LUAI_HASHLIMIT) bytes from a string to
** compute its hash
*/
// 最多会使用32(2^5)字节去计算一个字符串的哈希值
#if !defined(LUAI_HASHLIMIT)
#define LUAI_HASHLIMIT		5
#endif


/*
** equality for long strings
*/

// 长字符串相等判断
// 先比较两者指针是否相同 如果不同再比较实际的值是否相同
int luaS_eqlngstr (TString *a, TString *b) {
  size_t len = a->u.lnglen;
  lua_assert(a->tt == LUA_TLNGSTR && b->tt == LUA_TLNGSTR);
  // a和b指针相同 或者 取到的字符串值相同
  return (a == b) ||  /* same instance or... */
    ((len == b->u.lnglen) &&  /* equal length and ... */
     (memcmp(getstr(a), getstr(b), len) == 0));  /* equal contents */
}


// 求字符串的哈希值
unsigned int luaS_hash (const char *str, size_t l, unsigned int seed) {
  unsigned int h = seed ^ cast(unsigned int, l);
  // 求哈希时的步长, 长度小于32的时候步长是1, 之后每翻倍步长加一
  size_t step = (l >> LUAI_HASHLIMIT) + 1;
  for (; l >= step; l -= step)
    h ^= ((h<<5) + (h>>2) + cast_byte(str[l - 1]));
  return h;
}


// 计算并设置长字符串的哈希值 返回计算哈希值
unsigned int luaS_hashlongstr (TString *ts) {
  lua_assert(ts->tt == LUA_TLNGSTR);
  if (ts->extra == 0) {  /* no hash? */
    ts->hash = luaS_hash(getstr(ts), ts->u.lnglen, ts->hash);  // ts->hash初始被赋值为G(L)->seed
    ts->extra = 1;  /* now it has its hash 标记已经被计算了哈希值 */
  }
  return ts->hash;
}


/*
** resizes the string table
*/
// 为stringtable重新分配空间 可以扩大或缩小
// 空间扩大缩小后各个值存放的哈希桶也会一起重新计算
void luaS_resize (lua_State *L, int newsize) {
  int i;
  stringtable *tb = &G(L)->strt;
  if (newsize > tb->size) {  /* grow table if needed */
    // tb->hash 指向新的扩大后的空间
    luaM_reallocvector(L, tb->hash, tb->size, newsize, TString *);
    // 将增大的那部分空间初始化为NULL
    for (i = tb->size; i < newsize; i++)
      tb->hash[i] = NULL;
  }
  // 针对newsize对各个桶内的值重新计算应该属于哪个桶
  for (i = 0; i < tb->size; i++) {  /* rehash 遍历每一个桶 */
    TString *p = tb->hash[i];
    tb->hash[i] = NULL;
    while (p) {  /* for each node in the list 遍历当前桶的每一个节点 */
      TString *hnext = p->u.hnext;  /* save next 保存当前节点的下一个位置 */
      unsigned int h = lmod(p->hash, newsize);  /* new position 计算当前节点的应该位于哪个新的桶 */
      p->u.hnext = tb->hash[h];  /* chain it 插入新的桶之前保存新桶的头节点 */
      tb->hash[h] = p;  // 当前节点插入新的桶
      p = hnext;  // 访问当前桶的下一个节点
    }
  }
  if (newsize < tb->size) {  /* shrink table if needed */
    /* vanishing slice should be empty */
    // 上面已经针对newsize个桶进行了重新分配,缩小的部分不会有值
    lua_assert(tb->hash[newsize] == NULL && tb->hash[tb->size - 1] == NULL);
    luaM_reallocvector(L, tb->hash, tb->size, newsize, TString *);
  }
  tb->size = newsize;
}


/*
** Clear API string cache. (Entries cannot be empty, so fill them with
** a non-collectable string.)
*/
// 清理缓存g->strcache
void luaS_clearcache (global_State *g) {
  int i, j;
  for (i = 0; i < STRCACHE_N; i++)
    for (j = 0; j < STRCACHE_M; j++) {
    if (iswhite(g->strcache[i][j]))  /* will entry be collected? */
      g->strcache[i][j] = g->memerrmsg;  /* replace it with something fixed */
    }
}


/*
** Initialize the string table and the string cache
*/
// 初始化g->memerrmsg g->stringtable g->strcache
void luaS_init (lua_State *L) {
  global_State *g = G(L);
  int i, j;
  // 初始化stringtable
  luaS_resize(L, MINSTRTABSIZE);  /* initial size of string table */
  /* pre-create memory-error message */
  g->memerrmsg = luaS_newliteral(L, MEMERRMSG);
  luaC_fix(L, obj2gco(g->memerrmsg));  /* it should never be collected */
  // 初始化strcache
  for (i = 0; i < STRCACHE_N; i++)  /* fill cache with valid strings */
    for (j = 0; j < STRCACHE_M; j++)
      g->strcache[i][j] = g->memerrmsg;
}



/*
** creates a new string object
*/
// 创建一个空的TString对象,已经为将要存放的字符串申请了空间
// 由于不一定是长字符串还是短字符串所以没有指定长度
static TString *createstrobj (lua_State *L, size_t l, int tag, unsigned int h) {
  TString *ts;
  GCObject *o;
  size_t totalsize;  /* total size of TString object 一个TString对象的全部大小包括后面存储的字符串 */
  totalsize = sizelstring(l);
  o = luaC_newobj(L, tag, totalsize);
  ts = gco2ts(o);
  ts->hash = h;
  ts->extra = 0;  // 长字符串标记还没有计算哈希值 短字符串标记不是关键字
  getstr(ts)[l] = '\0';  /* ending 0 标记字符串结尾 */
  return ts;
}


// 创建长字符串 在对象中明确指定字符串的长度
TString *luaS_createlngstrobj (lua_State *L, size_t l) {
  TString *ts = createstrobj(L, l, LUA_TLNGSTR, G(L)->seed);
  ts->u.lnglen = l;
  return ts;
}


// 从stringtable中删除ts
void luaS_remove (lua_State *L, TString *ts) {
  stringtable *tb = &G(L)->strt;
  TString **p = &tb->hash[lmod(ts->hash, tb->size)];  // 找到删除的对象所在的桶
  while (*p != ts)  /* find previous element */
    p = &(*p)->u.hnext;
  // 从链表中删除元素可以使用二级指针
  // 现在p就是ts上一个节点的netx域的指针
  // *p==ts 修改*p=(*p)->next 自然它的上一个节点也会修改
  *p = (*p)->u.hnext;  /* remove element from its list */
  tb->nuse--;
}


/*
** checks whether short string exists and reuses it or creates a new one
*/
// 创建短字符串
// 查询短字符串是否存在 存在的话复用 不存在就新建
static TString *internshrstr (lua_State *L, const char *str, size_t l) {
  TString *ts;
  global_State *g = G(L);
  unsigned int h = luaS_hash(str, l, g->seed);
  TString **list = &g->strt.hash[lmod(h, g->strt.size)];  // 找到所在的桶
  lua_assert(str != NULL);  /* otherwise 'memcmp'/'memcpy' are undefined */
  for (ts = *list; ts != NULL; ts = ts->u.hnext) {  // 遍历桶的节点
    if (l == ts->shrlen &&
        (memcmp(str, getstr(ts), l * sizeof(char)) == 0)) {
      /* found! 长度相同 内容完全一样 */
      if (isdead(g, ts))  /* dead (but not collected yet)? */
        changewhite(ts);  /* resurrect it */
      return ts;  // 直接复用
    }
  }
  // 检查stringtable的空间是否足够
  if (g->strt.nuse >= g->strt.size && g->strt.size <= MAX_INT/2) {
    luaS_resize(L, g->strt.size * 2);  // 翻倍扩容
    list = &g->strt.hash[lmod(h, g->strt.size)];  /* recompute with new size 扩容后需要重新计算桶 */
  }
  // 创建新的短字符串
  ts = createstrobj(L, l, LUA_TSHRSTR, h);
  memcpy(getstr(ts), str, l * sizeof(char));
  ts->shrlen = cast_byte(l);
  ts->u.hnext = *list;
  *list = ts;
  g->strt.nuse++;
  return ts;
}


/*symotion-prefix)
** new string (with explicit length)
*/
// 创建字符串(显式指定长度)
// 这个函数不会查询缓存
TString *luaS_newlstr (lua_State *L, const char *str, size_t l) {
  if (l <= LUAI_MAXSHORTLEN)  /* short string? 创建短字符串 */
    return internshrstr(L, str, l);
  else {  // 创建长字符串
    TString *ts;
    if (l >= (MAX_SIZE - sizeof(TString))/sizeof(char))
      luaM_toobig(L);
    ts = luaS_createlngstrobj(L, l);
    memcpy(getstr(ts), str, l * sizeof(char));
    return ts;
  }
}


/*
** Create or reuse a zero-terminated string, first checking in the
** cache (using the string address as a key). The cache can contain
** only zero-terminated strings, so it is safe to use 'strcmp' to
** check hits.
*/
// 使用这个函数创建字符串直接去cache中找,有相同的就不再创建
// cache在同一个桶内是先进先出的 进来一个把最后一个挤出去
TString *luaS_new (lua_State *L, const char *str) {
  unsigned int i = point2uint(str) % STRCACHE_N;  /* hash */
  int j;
  TString **p = G(L)->strcache[i];
  for (j = 0; j < STRCACHE_M; j++) {
    if (strcmp(str, getstr(p[j])) == 0)  /* hit? */
      return p[j];  /* that is it */
  }
  /* normal route */
  // 在cache中没找到 在新创建之前先从cache中删掉一个用来存新创建的
  for (j = STRCACHE_M - 1; j > 0; j--)
    p[j] = p[j - 1];  /* move out last element 全部向后移一个 最终删掉最后一个 */
  /* new element is first in the list */
  p[0] = luaS_newlstr(L, str, strlen(str));
  return p[0];
}


// userdata就是一块原始内存 没有存放任何东西
Udata *luaS_newudata (lua_State *L, size_t s) {
  Udata *u;
  GCObject *o;
  if (s > MAX_SIZE - sizeof(Udata))
    luaM_toobig(L);
  o = luaC_newobj(L, LUA_TUSERDATA, sizeludata(s));
  u = gco2u(o);
  u->len = s;
  u->metatable = NULL;
  setuservalue(L, u, luaO_nilobject);
  return u;
}

