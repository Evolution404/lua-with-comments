/*
** $Id: lvm.c,v 2.268 2016/02/05 19:59:14 roberto Exp $
** Lua virtual machine
** See Copyright Notice in lua.h
*/

#define lvm_c
#define LUA_CORE

#include "lprefix.h"

#include <float.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lua.h"

#include "ldebug.h"
#include "ldo.h"
#include "lfunc.h"
#include "lgc.h"
#include "lobject.h"
#include "lopcodes.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "ltm.h"
#include "lvm.h"


/* limit for table tag-method chains (to avoid loops) */
#define MAXTAGLOOP	2000



/*
** 'l_intfitsf' checks whether a given integer can be converted to a
** float without rounding. Used in comparisons. Left undefined if
** all integers fit in a float precisely.
*/
#if !defined(l_intfitsf)

/* number of bits in the mantissa of a float */
#define NBM		(l_mathlim(MANT_DIG))

/*
** Check whether some integers may not fit in a float, that is, whether
** (maxinteger >> NBM) > 0 (that implies (1 << NBM) <= maxinteger).
** (The shifts are done in parts to avoid shifting by more than the size
** of an integer. In a worst case, NBM == 113 for long double and
** sizeof(integer) == 32.)
*/
#if ((((LUA_MAXINTEGER >> (NBM / 4)) >> (NBM / 4)) >> (NBM / 4)) \
	>> (NBM - (3 * (NBM / 4))))  >  0

#define l_intfitsf(i)  \
  (-((lua_Integer)1 << NBM) <= (i) && (i) <= ((lua_Integer)1 << NBM))

#endif

#endif



/*
** Try to convert a value to a float. The float case is already handled
** by the macro 'tonumber'.
*/
// 负责从整数和字符串转换成浮点数 转换成功返回1 失败返回0
int luaV_tonumber_ (const TValue *obj, lua_Number *n) {
  TValue v;
  if (ttisinteger(obj)) {  // 整数类型直接转换
    *n = cast_num(ivalue(obj));
    return 1;
  }
  // cvt2num 由宏LUA_NOCVTS2N控制 定义了这个宏lua将关闭自动从字符串转换成数字
  else if (cvt2num(obj) &&  /* string convertible to number? 字符串是否可以转换成数字 */
            // luaO_str2num返回的是转换的字符串大小包括\0 vslen返回的是字符串长度不包括\0
            luaO_str2num(svalue(obj), &v) == vslen(obj) + 1) {
    *n = nvalue(&v);  /* convert result of 'luaO_str2num' to a float */
    return 1;
  }
  else
    return 0;  /* conversion failed */
}


/*
** try to convert a value to an integer, rounding according to 'mode':
** mode == 0: accepts only integral values  只接受整数也包括字符串类型的整数
** mode == 1: takes the floor of the number 将浮点数向下取整
** mode == 2: takes the ceil of the number  将浮点数向上取整
*/
// 返回值 0转换失败 1转换成功
int luaV_tointeger (const TValue *obj, lua_Integer *p, int mode) {
  TValue v;
 again:
  if (ttisfloat(obj)) {  // 传入了浮点数
    lua_Number n = fltvalue(obj);
    lua_Number f = l_floor(n);  // 求出floor后的值
    if (n != f) {  /* not an integral value? */
      if (mode == 0) return 0;  /* fails if mode demands integral value */
      else if (mode > 1)  /* needs ceil? */
        f += 1;  /* convert floor to ceil (remember: n != f) mode是2代表取ceil也就是floor+1 */
    }
    // 从double类型转换成long long类型, 转换过程还进行了范围判断
    return lua_numbertointeger(f, p);
  }
  else if (ttisinteger(obj)) {  // 传入了整数
    *p = ivalue(obj);
    return 1;
  }
  // 判断字符串能转换成浮点数
  else if (cvt2num(obj) &&
            luaO_str2num(svalue(obj), &v) == vslen(obj) + 1) {
    // 先尝试转换成数字 跳转回again处再使用整数浮点数的处理方法
    obj = &v;
    goto again;  /* convert result from 'luaO_str2num' to an integer */
  }
  return 0;  /* conversion failed */
}


/*
** Try to convert a 'for' limit to an integer, preserving the
** semantics of the loop.
** (The following explanation assumes a non-negative step; it is valid
** for negative steps mutatis mutandis.)
** If the limit can be converted to an integer, rounding down, that is
** it.
** Otherwise, check whether the limit can be converted to a number.  If
** the number is too large, it is OK to set the limit as LUA_MAXINTEGER,
** which means no limit.  If the number is too negative, the loop
** should not run, because any initial integer value is larger than the
** limit. So, it sets the limit to LUA_MININTEGER. 'stopnow' corrects
** the extreme case when the initial value is LUA_MININTEGER, in which
** case the LUA_MININTEGER limit would still run the loop once.
*/
// lua for循环 结构
// for var=exp1,exp2,exp3 do  
//    <执行体>  
// end 
// exp1是初始值 exp2是目标值 exp3是步长 exp3可选默认为1
static int forlimit (const TValue *obj, lua_Integer *p, lua_Integer step,
                     int *stopnow) {
  *stopnow = 0;  /* usually, let loops run */
  if (!luaV_tointeger(obj, p, (step < 0 ? 2 : 1))) {  /* not fit in integer? */
    lua_Number n;  /* try to convert to float */
    if (!tonumber(obj, &n)) /* cannot convert to float? */
      return 0;  /* not a number */
    if (luai_numlt(0, n)) {  /* if true, float is larger than max integer */
      *p = LUA_MAXINTEGER;
      if (step < 0) *stopnow = 1;
    }
    else {  /* float is smaller than min integer */
      *p = LUA_MININTEGER;
      if (step >= 0) *stopnow = 1;
    }
  }
  return 1;
}


/*
** Finish the table access 'val = t[key]'.
** if 'slot' is NULL, 't' is not a table; otherwise, 'slot' points to
** t[k] entry (which must be nil).
*/
// 直接获取t[k]没有得到值 开始搜索元方法
// t被查询的值 key要查询的键 val查询结果放置位置 slot用来标记t类型

// slot是NULL说明t不是table slot是nil说明t[k]为nil
// 调用这个方法传入的slot都是 luaV_fastget作用后的结果

// t不是表 查询基本类型元表的__index项 设为tm
// t是表   查询该表的元表的__index项   设为tm
// tm是函数 调用tm
// tm不是函数 使用fastget获取key键得值 获取到设置val
//   获取不到 t=tm 返回第一步
void luaV_finishget (lua_State *L, const TValue *t, TValue *key, StkId val,
                      const TValue *slot) {
  int loop;  /* counter to avoid infinite loops */
  const TValue *tm;  /* metamethod */
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    // 获取t的__index元方法, t是不是表类型获取方法不同
    if (slot == NULL) {  /* 't' is not a table? slot是NULL说明t不是table */
      lua_assert(!ttistable(t));
      tm = luaT_gettmbyobj(L, t, TM_INDEX);
      if (ttisnil(tm))
        luaG_typeerror(L, t, "index");  /* no metamethod */
      /* else will try the metamethod */
    }
    else {  /* 't' is a table slot是nil说明t[k]是nil*/
      lua_assert(ttisnil(slot));
      // hvalue(t)->metatable 是t的元表 下面用来获取元表中的元方法
      tm = fasttm(L, hvalue(t)->metatable, TM_INDEX);  /* table's metamethod */
      if (tm == NULL) {  /* no metamethod? */
        setnilvalue(val);  /* result is nil */
        return;
      }
      /* else will try the metamethod */
    }
    // __index项是一个函数 调用这个函数
    if (ttisfunction(tm)) {  /* is metamethod a function? */
      // 调用__index函数会传入table,key两个参数 也就是t和key
      // 传入t和key,返回值设置到val里
      luaT_callTM(L, tm, t, key, val, 1);  /* call it */
      return;
    }
    // 获取的结果不是函数继续查询__index项
    t = tm;  /* else try to access 'tm[key]' */
    if (luaV_fastget(L,t,key,slot,luaH_get)) {  /* fast track? */
      setobj2s(L, val, slot);  /* done */
      return;
    }
    /* else repeat (tail call 'luaV_finishget') */
  }
  luaG_runerror(L, "'__index' chain too long; possible loop");
}


/*
** Finish a table assignment 't[key] = val'.
** If 'slot' is NULL, 't' is not a table.  Otherwise, 'slot' points
** to the entry 't[key]', or to 'luaO_nilobject' if there is no such
** entry.  (The value at 'slot' must be nil, otherwise 'luaV_fastset'
** would have done the job.)
*/
// t[key] = val
// slot的意义与上面函数一致
// 查询__newindex元方法, 如果是
void luaV_finishset (lua_State *L, const TValue *t, TValue *key,
                     StkId val, const TValue *slot) {
  int loop;  /* counter to avoid infinite loops */
  for (loop = 0; loop < MAXTAGLOOP; loop++) {
    const TValue *tm;  /* '__newindex' metamethod */
    // 查询__newindex元方法
    if (slot != NULL) {  /* is 't' a table? t是表 */
      Table *h = hvalue(t);  /* save 't' table */
      lua_assert(ttisnil(slot));  /* old value must be nil */
      tm = fasttm(L, h->metatable, TM_NEWINDEX);  /* get metamethod */
      // 没有查询到元方法那么就直接在表上创建新的key
      if (tm == NULL) {  /* no metamethod? */
        if (slot == luaO_nilobject)  /* no previous entry? slot必定是nil */
          slot = luaH_newkey(L, h, key);  /* create one 返回的新Node的val字段 */
        /* no metamethod and (now) there is an entry with given key */
        setobj2t(L, cast(TValue *, slot), val);  /* set its new value 直接往新Node上赋值 */
        invalidateTMcache(h);
        luaC_barrierback(L, h, val);
        return;
      }
      /* else will try the metamethod */
    }
    else {  /* not a table; check metamethod t不是表 查询基本类型 */
      if (ttisnil(tm = luaT_gettmbyobj(L, t, TM_NEWINDEX)))
        luaG_typeerror(L, t, "index");
    }
    /* try the metamethod */
    if (ttisfunction(tm)) {  // __newindex字段是函数 调用这个函数
      // __newindex传入三个参数 没有返回值
      luaT_callTM(L, tm, t, key, val, 0);
      return;
    }
    t = tm;  /* else repeat assignment over 'tm' */
    if (luaV_fastset(L, t, key, slot, luaH_get, val))  // 类似get继续递归向内查询
      return;  /* done */
    /* else loop */
  }
  luaG_runerror(L, "'__newindex' chain too long; possible loop");
}


/*
** Compare two strings 'ls' x 'rs', returning an integer smaller-equal-
** -larger than zero if 'ls' is smaller-equal-larger than 'rs'.
** The code is a little tricky because it allows '\0' in the strings
** and it uses 'strcoll' (to respect locales) for each segments
** of the strings.
*/
// 比较两个TString对象的大小
static int l_strcmp (const TString *ls, const TString *rs) {
  const char *l = getstr(ls);
  size_t ll = tsslen(ls);
  const char *r = getstr(rs);
  size_t lr = tsslen(rs);
  for (;;) {  /* for each segment */
    int temp = strcoll(l, r);
    if (temp != 0)  /* not equal? */
      return temp;  /* done */
    else {  /* strings are equal up to a '\0' */
      size_t len = strlen(l);  /* index of first '\0' in both strings */
      if (len == lr)  /* 'rs' is finished? */
        return (len == ll) ? 0 : 1;  /* check 'ls' */
      else if (len == ll)  /* 'ls' is finished? */
        return -1;  /* 'ls' is smaller than 'rs' ('rs' is not finished) */
      /* both strings longer than 'len'; go on comparing after the '\0' */
      len++;
      l += len; ll -= len; r += len; lr -= len;
    }
  }
}


/*
** Check whether integer 'i' is less than float 'f'. If 'i' has an
** exact representation as a float ('l_intfitsf'), compare numbers as
** floats. Otherwise, if 'f' is outside the range for integers, result
** is trivial. Otherwise, compare them as integers. (When 'i' has no
** float representation, either 'f' is "far away" from 'i' or 'f' has
** no precision left for a fractional part; either way, how 'f' is
** truncated is irrelevant.) When 'f' is NaN, comparisons must result
** in false.
*/
static int LTintfloat (lua_Integer i, lua_Number f) {
#if defined(l_intfitsf)
  if (!l_intfitsf(i)) {
    if (f >= -cast_num(LUA_MININTEGER))  /* -minint == maxint + 1 */
      return 1;  /* f >= maxint + 1 > i */
    else if (f > cast_num(LUA_MININTEGER))  /* minint < f <= maxint ? */
      return (i < cast(lua_Integer, f));  /* compare them as integers */
    else  /* f <= minint <= i (or 'f' is NaN)  -->  not(i < f) */
      return 0;
  }
#endif
  return luai_numlt(cast_num(i), f);  /* compare them as floats */
}


/*
** Check whether integer 'i' is less than or equal to float 'f'.
** See comments on previous function.
*/
static int LEintfloat (lua_Integer i, lua_Number f) {
#if defined(l_intfitsf)
  if (!l_intfitsf(i)) {
    if (f >= -cast_num(LUA_MININTEGER))  /* -minint == maxint + 1 */
      return 1;  /* f >= maxint + 1 > i */
    else if (f >= cast_num(LUA_MININTEGER))  /* minint <= f <= maxint ? */
      return (i <= cast(lua_Integer, f));  /* compare them as integers */
    else  /* f < minint <= i (or 'f' is NaN)  -->  not(i <= f) */
      return 0;
  }
#endif
  return luai_numle(cast_num(i), f);  /* compare them as floats */
}


/*
** Return 'l < r', for numbers.
*/
static int LTnum (const TValue *l, const TValue *r) {
  if (ttisinteger(l)) {
    lua_Integer li = ivalue(l);
    if (ttisinteger(r))
      return li < ivalue(r);  /* both are integers */
    else  /* 'l' is int and 'r' is float */
      return LTintfloat(li, fltvalue(r));  /* l < r ? */
  }
  else {
    lua_Number lf = fltvalue(l);  /* 'l' must be float */
    if (ttisfloat(r))
      return luai_numlt(lf, fltvalue(r));  /* both are float */
    else if (luai_numisnan(lf))  /* 'r' is int and 'l' is float */
      return 0;  /* NaN < i is always false */
    else  /* without NaN, (l < r)  <-->  not(r <= l) */
      return !LEintfloat(ivalue(r), lf);  /* not (r <= l) ? */
  }
}


/*
** Return 'l <= r', for numbers.
*/
static int LEnum (const TValue *l, const TValue *r) {
  if (ttisinteger(l)) {
    lua_Integer li = ivalue(l);
    if (ttisinteger(r))
      return li <= ivalue(r);  /* both are integers */
    else  /* 'l' is int and 'r' is float */
      return LEintfloat(li, fltvalue(r));  /* l <= r ? */
  }
  else {
    lua_Number lf = fltvalue(l);  /* 'l' must be float */
    if (ttisfloat(r))
      return luai_numle(lf, fltvalue(r));  /* both are float */
    else if (luai_numisnan(lf))  /* 'r' is int and 'l' is float */
      return 0;  /*  NaN <= i is always false */
    else  /* without NaN, (l <= r)  <-->  not(r < l) */
      return !LTintfloat(ivalue(r), lf);  /* not (r < l) ? */
  }
}


/*
** Main operation less than; return 'l < r'.
*/
// 都是数字,字符串可以直接比较
// 都是表查询元表进行比较
int luaV_lessthan (lua_State *L, const TValue *l, const TValue *r) {
  int res;
  if (ttisnumber(l) && ttisnumber(r))  /* both operands are numbers? */
    return LTnum(l, r);
  else if (ttisstring(l) && ttisstring(r))  /* both are strings? */
    return l_strcmp(tsvalue(l), tsvalue(r)) < 0;
  else if ((res = luaT_callorderTM(L, l, r, TM_LT)) < 0)  /* no metamethod? */
    luaG_ordererror(L, l, r);  /* error */
  return res;
}


/*
** Main operation less than or equal to; return 'l <= r'. If it needs
** a metamethod and there is no '__le', try '__lt', based on
** l <= r iff !(r < l) (assuming a total order). If the metamethod
** yields during this substitution, the continuation has to know
** about it (to negate the result of r<l); bit CIST_LEQ in the call
** status keeps that information.
*/
int luaV_lessequal (lua_State *L, const TValue *l, const TValue *r) {
  int res;
  if (ttisnumber(l) && ttisnumber(r))  /* both operands are numbers? */
    return LEnum(l, r);
  else if (ttisstring(l) && ttisstring(r))  /* both are strings? */
    return l_strcmp(tsvalue(l), tsvalue(r)) <= 0;
  else if ((res = luaT_callorderTM(L, l, r, TM_LE)) >= 0)  /* try 'le' */
    return res;
  else {  /* try 'lt': */
    L->ci->callstatus |= CIST_LEQ;  /* mark it is doing 'lt' for 'le' */
    res = luaT_callorderTM(L, r, l, TM_LT);
    L->ci->callstatus ^= CIST_LEQ;  /* clear mark */
    if (res < 0)
      luaG_ordererror(L, l, r);
    return !res;  /* result is negated */
  }
}


/*
** Main operation for equality of Lua values; return 't1 == t2'.
** L == NULL means raw equality (no metamethods)
*/
// 比较t1和t2是否相等
// L==NULL 直接比较不查询元表
// L!=NULL 复杂类型还会查询元表__eq方法进行比较
int luaV_equalobj (lua_State *L, const TValue *t1, const TValue *t2) {
  const TValue *tm;
  if (ttype(t1) != ttype(t2)) {  /* not the same variant? 两个数据不是完全一样的类型 */
    // 四种可能 1:t1t2是数字 2:t1是数字t2不是 3:t1不是数字t2是数字 4:t1t2不是数字
    // 2和3主类型不同 4则t1不是数字 所以只剩1 t1t2都是数字
    if (ttnov(t1) != ttnov(t2) || ttnov(t1) != LUA_TNUMBER)  // 两者主类型不一样 或 t1不是数字类型
      return 0;  /* only numbers can be equal with different variants */
    else {  /* two numbers with different variants */
      lua_Integer i1, i2;  /* compare them as integers */
      return (tointeger(t1, &i1) && tointeger(t2, &i2) && i1 == i2);
    }
  }
  /* values have same type and same variant */
  // t1和t2的类型完全一致
  switch (ttype(t1)) {
    case LUA_TNIL: return 1;
    case LUA_TNUMINT: return (ivalue(t1) == ivalue(t2));
    case LUA_TNUMFLT: return luai_numeq(fltvalue(t1), fltvalue(t2));
    case LUA_TBOOLEAN: return bvalue(t1) == bvalue(t2);  /* true must be 1 !! */
    case LUA_TLIGHTUSERDATA: return pvalue(t1) == pvalue(t2);
    case LUA_TLCF: return fvalue(t1) == fvalue(t2);
    case LUA_TSHRSTR: return eqshrstr(tsvalue(t1), tsvalue(t2));
    case LUA_TLNGSTR: return luaS_eqlngstr(tsvalue(t1), tsvalue(t2));
    case LUA_TUSERDATA: {  // 需要查询元表
      if (uvalue(t1) == uvalue(t2)) return 1;
      else if (L == NULL) return 0;
      tm = fasttm(L, uvalue(t1)->metatable, TM_EQ);
      if (tm == NULL)
        tm = fasttm(L, uvalue(t2)->metatable, TM_EQ);
      break;  /* will try TM */
    }
    case LUA_TTABLE: {  // 需要查询元表
      if (hvalue(t1) == hvalue(t2)) return 1;
      else if (L == NULL) return 0;
      tm = fasttm(L, hvalue(t1)->metatable, TM_EQ);
      if (tm == NULL)
        tm = fasttm(L, hvalue(t2)->metatable, TM_EQ);
      break;  /* will try TM */
    }
    default:
      return gcvalue(t1) == gcvalue(t2);
  }
  if (tm == NULL)  /* no TM? */
    return 0;  /* objects are different */
  // 调用元方法进行比较 比较结果放入top 不是top-1 因为这个结果函数返回后就不用了
  luaT_callTM(L, tm, t1, t2, L->top, 1);  /* call TM */
  return !l_isfalse(L->top);
}


/* macro used by 'luaV_concat' to ensure that element at 'o' is a string */
// 确保o一定是一个字符串 如果不是字符串也会转换成字符串
#define tostring(L,o)  \
	(ttisstring(o) || (cvt2str(o) && (luaO_tostring(L, o), 1)))

#define isemptystr(o)	(ttisshrstring(o) && tsvalue(o)->shrlen == 0)

/* copy strings in stack from top - n up to top - 1 to buffer */
// 拷贝 top-1 top-2 ... top-n位置的字符串到buff里
static void copy2buff (StkId top, int n, char *buff) {
  size_t tl = 0;  /* size already copied */
  do {
    size_t l = vslen(top - n);  /* length of string being copied */
    memcpy(buff + tl, svalue(top - n), l * sizeof(char));
    tl += l;
  } while (--n > 0);
}


/*
** Main operation for concatenation: concat 'total' values in the stack,
** from 'L->top - total' up to 'L->top - 1'.
*/
// 将 top-1 top-2 ... top-total上的字符串进行合并
// 执行完毕新的top-1就放着合并后的结果
void luaV_concat (lua_State *L, int total) {
  lua_assert(total >= 2);
  do {
    StkId top = L->top;
    int n = 2;  /* number of elements handled in this pass (at least 2) */
    if (!(ttisstring(top-2) || cvt2str(top-2)) || !tostring(L, top-1))  // 运算的两个值有一个不是字符串类型
      luaT_trybinTM(L, top-2, top-1, top-2, TM_CONCAT);
    // 运算结果放在 top-2 的位置
    else if (isemptystr(top - 1))  /* second operand is empty? */
      cast_void(tostring(L, top - 2));  /* result is first operand top-1是空结果就是top-2 */
    else if (isemptystr(top - 2)) {  /* first operand is an empty string? */
      setobjs2s(L, top - 2, top - 1);  /* result is second op. top-2是空结果是top-1 拷贝到top-2位置 */
    }
    else {
      // 连接两个字符串类型
      /* at least two non-empty string values; get as many as possible */
      size_t tl = vslen(top - 1);  // tl最开始等于top-1字符串的长度
      TString *ts;
      /* collect total length and number of strings */
      // 从top-1在栈上不断向前搜索 计算合并所有字符串后的长度tl(total len)
      for (n = 1; n < total && tostring(L, top - n - 1); n++) {
        size_t l = vslen(top - n - 1);
        if (l >= (MAX_SIZE/sizeof(char)) - tl)
          luaG_runerror(L, "string length overflow");
        tl += l;
      }
      if (tl <= LUAI_MAXSHORTLEN) {  /* is result a short string? 这是一个短字符串 */
        char buff[LUAI_MAXSHORTLEN];
        copy2buff(top, n, buff);  /* copy strings to buffer 把这几个字符串拷贝到buff里 */
        ts = luaS_newlstr(L, buff, tl);  // 创建新字符串
      }
      else {  /* long string; copy strings directly to final result 长字符串 */
        // 为什么不使用上面短字符串的创建buff再拷贝到buff里 使用luaS_newlstr创建字符串?
        // 这里如果使用 char buff[tl] 然后再拷贝进去会有可能造成栈溢出
        // 如果使用malloc再free就不如使用下面的方式了
        ts = luaS_createlngstrobj(L, tl);  // 创建字符串对象 申请字符串的空间
        copy2buff(top, n, getstr(ts));     // 把实际内容拷贝进去
      }
      setsvalue2s(L, top - n, ts);  /* create result */
    }
    total -= n-1;  /* got 'n' strings to create 1 new 这一轮已经合并了n-1个了 */
    L->top -= n-1;  /* popped 'n' strings and pushed one top要向前移动n-1个 */
  } while (total > 1);  /* repeat until only 1 result left */
}


/*
** Main operation 'ra' = #rb'.
*/
void luaV_objlen (lua_State *L, StkId ra, const TValue *rb) {
  const TValue *tm;
  switch (ttype(rb)) {
    case LUA_TTABLE: {
      Table *h = hvalue(rb);
      tm = fasttm(L, h->metatable, TM_LEN);
      if (tm) break;  /* metamethod? break switch to call it */
      setivalue(ra, luaH_getn(h));  /* else primitive len */
      return;
    }
    case LUA_TSHRSTR: {
      setivalue(ra, tsvalue(rb)->shrlen);
      return;
    }
    case LUA_TLNGSTR: {
      setivalue(ra, tsvalue(rb)->u.lnglen);
      return;
    }
    default: {  /* try metamethod */
      tm = luaT_gettmbyobj(L, rb, TM_LEN);
      if (ttisnil(tm))  /* no metamethod? */
        luaG_typeerror(L, rb, "get length of");
      break;
    }
  }
  luaT_callTM(L, tm, rb, rb, ra, 1);
}


/*
** Integer division; return 'm // n', that is, floor(m/n).
** C division truncates its result (rounds towards zero).
** 'floor(q) == trunc(q)' when 'q >= 0' or when 'q' is integer,
** otherwise 'floor(q) == trunc(q) - 1'.
*/
lua_Integer luaV_div (lua_State *L, lua_Integer m, lua_Integer n) {
  if (l_castS2U(n) + 1u <= 1u) {  /* special cases: -1 or 0 */
    if (n == 0)
      luaG_runerror(L, "attempt to divide by zero");
    return intop(-, 0, m);   /* n==-1; avoid overflow with 0x80000...//-1 */
  }
  else {
    lua_Integer q = m / n;  /* perform C division */
    if ((m ^ n) < 0 && m % n != 0)  /* 'm/n' would be negative non-integer? */
      q -= 1;  /* correct result for different rounding */
    return q;
  }
}


/*
** Integer modulus; return 'm % n'. (Assume that C '%' with
** negative operands follows C99 behavior. See previous comment
** about luaV_div.)
*/
lua_Integer luaV_mod (lua_State *L, lua_Integer m, lua_Integer n) {
  if (l_castS2U(n) + 1u <= 1u) {  /* special cases: -1 or 0 */
    if (n == 0)
      luaG_runerror(L, "attempt to perform 'n%%0'");
    return 0;   /* m % -1 == 0; avoid overflow with 0x80000...%-1 */
  }
  else {
    lua_Integer r = m % n;
    if (r != 0 && (m ^ n) < 0)  /* 'm/n' would be non-integer negative? */
      r += n;  /* correct result for different rounding */
    return r;
  }
}


/* number of bits in an integer */
#define NBITS	cast_int(sizeof(lua_Integer) * CHAR_BIT)

/*
** Shift left operation. (Shift right just negates 'y'.)
*/
lua_Integer luaV_shiftl (lua_Integer x, lua_Integer y) {
  if (y < 0) {  /* shift right? */
    if (y <= -NBITS) return 0;
    else return intop(>>, x, -y);
  }
  else {  /* shift left */
    if (y >= NBITS) return 0;
    else return intop(<<, x, y);
  }
}


/*
** check whether cached closure in prototype 'p' may be reused, that is,
** whether there is a cached closure with the same upvalues needed by
** new closure to be created.
*/
static LClosure *getcached (Proto *p, UpVal **encup, StkId base) {
  LClosure *c = p->cache;
  // 如果有缓存的闭包 检查这个缓存的闭包的upvalue是否与当前需要的完全一致
  if (c != NULL) {  /* is there a cached closure? */
    int nup = p->sizeupvalues;
    Upvaldesc *uv = p->upvalues;
    int i;
    for (i = 0; i < nup; i++) {  /* check whether it has right upvalues */
      TValue *v = uv[i].instack ? base + uv[i].idx : encup[uv[i].idx]->v;
      if (c->upvals[i]->v != v)
        return NULL;  /* wrong upvalue; cannot reuse closure */
    }
  }
  return c;  /* return cached closure (or NULL if no cached closure) */
}


/*
** create a new Lua closure, push it in the stack, and initialize
** its upvalues. Note that the closure is not cached if prototype is
** already black (which means that 'cache' was already cleared by the
** GC).
*/
static void pushclosure (lua_State *L, Proto *p, UpVal **encup, StkId base,
                         StkId ra) {
  int nup = p->sizeupvalues;
  Upvaldesc *uv = p->upvalues;
  int i;
  // 创建一个新的lua函数闭包,写入OP_CLOSURE的参数A位置
  LClosure *ncl = luaF_newLclosure(L, nup);
  // 新建的闭包的与传入的原型联系起来
  ncl->p = p;
  setclLvalue(L, ra, ncl);  /* anchor new closure in stack */
  // 为新建的闭包写入upvalue信息
  for (i = 0; i < nup; i++) {  /* fill in its upvalues */
    if (uv[i].instack)  /* upvalue refers to local variable? */
      ncl->upvals[i] = luaF_findupval(L, base + uv[i].idx);
    else  /* get upvalue from enclosing function */
      ncl->upvals[i] = encup[uv[i].idx];
    ncl->upvals[i]->refcount++;
    /* new closure is white, so we do not need a barrier here */
  }
  // 在原型上缓存新建的闭包,getcached函数复用
  // 如果继续调用这个函数就不用重新创建了
  if (!isblack(p))  /* cache will not break GC invariant? */
    p->cache = ncl;  /* save it on cache for reuse */
}


/*
** finish execution of an opcode interrupted by an yield
*/
void luaV_finishOp (lua_State *L) {
  CallInfo *ci = L->ci;
  StkId base = ci->u.l.base;
  Instruction inst = *(ci->u.l.savedpc - 1);  /* interrupted instruction */
  OpCode op = GET_OPCODE(inst);
  switch (op) {  /* finish its execution */
    case OP_ADD: case OP_SUB: case OP_MUL: case OP_DIV: case OP_IDIV:
    case OP_BAND: case OP_BOR: case OP_BXOR: case OP_SHL: case OP_SHR:
    case OP_MOD: case OP_POW:
    case OP_UNM: case OP_BNOT: case OP_LEN:
    case OP_GETTABUP: case OP_GETTABLE: case OP_SELF: {
      setobjs2s(L, base + GETARG_A(inst), --L->top);
      break;
    }
    case OP_LE: case OP_LT: case OP_EQ: {
      int res = !l_isfalse(L->top - 1);
      L->top--;
      if (ci->callstatus & CIST_LEQ) {  /* "<=" using "<" instead? */
        lua_assert(op == OP_LE);
        ci->callstatus ^= CIST_LEQ;  /* clear mark */
        res = !res;  /* negate result */
      }
      lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_JMP);
      if (res != GETARG_A(inst))  /* condition failed? */
        ci->u.l.savedpc++;  /* skip jump instruction */
      break;
    }
    case OP_CONCAT: {
      StkId top = L->top - 1;  /* top when 'luaT_trybinTM' was called */
      int b = GETARG_B(inst);      /* first element to concatenate */
      int total = cast_int(top - 1 - (base + b));  /* yet to concatenate */
      setobj2s(L, top - 2, top);  /* put TM result in proper position */
      if (total > 1) {  /* are there elements to concat? */
        L->top = top - 1;  /* top is one after last element (at top-2) */
        luaV_concat(L, total);  /* concat them (may yield again) */
      }
      /* move final result to final position */
      setobj2s(L, ci->u.l.base + GETARG_A(inst), L->top - 1);
      L->top = ci->top;  /* restore top */
      break;
    }
    case OP_TFORCALL: {
      lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_TFORLOOP);
      L->top = ci->top;  /* correct top */
      break;
    }
    case OP_CALL: {
      if (GETARG_C(inst) - 1 >= 0)  /* nresults >= 0? */
        L->top = ci->top;  /* adjust results */
      break;
    }
    case OP_TAILCALL: case OP_SETTABUP: case OP_SETTABLE:
      break;
    default: lua_assert(0);
  }
}




/*
** {==================================================================
** Function 'luaV_execute': main interpreter loop
** ===================================================================
*/


/*
** some macros for common tasks in 'luaV_execute'
*/


#define RA(i)	(base+GETARG_A(i))
#define RB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgR, base+GETARG_B(i))
#define RC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgR, base+GETARG_C(i))
#define RKB(i)	check_exp(getBMode(GET_OPCODE(i)) == OpArgK, \
	ISK(GETARG_B(i)) ? k+INDEXK(GETARG_B(i)) : base+GETARG_B(i))
#define RKC(i)	check_exp(getCMode(GET_OPCODE(i)) == OpArgK, \
	ISK(GETARG_C(i)) ? k+INDEXK(GETARG_C(i)) : base+GETARG_C(i))


/* execute a jump instruction */
#define dojump(ci,i,e) \
  { int a = GETARG_A(i); \
    if (a != 0) luaF_close(L, ci->u.l.base + a - 1); \
    ci->u.l.savedpc += GETARG_sBx(i) + e; }

/* for test instructions, execute the jump instruction that follows it */
#define donextjump(ci)	{ i = *ci->u.l.savedpc; dojump(ci, i, 1); }


#define Protect(x)	{ {x;}; base = ci->u.l.base; }

#define checkGC(L,c)  \
	{ luaC_condGC(L, L->top = (c),  /* limit of live values */ \
                         Protect(L->top = ci->top));  /* restore top */ \
           luai_threadyield(L); }


/* fetch an instruction and prepare its execution */
#define vmfetch()	{ \
  i = *(ci->u.l.savedpc++); \
  if (L->hookmask & (LUA_MASKLINE | LUA_MASKCOUNT)) \
    Protect(luaG_traceexec(L)); \
  ra = RA(i); /* WARNING: any stack reallocation invalidates 'ra' */ \
  lua_assert(base == ci->u.l.base); \
  lua_assert(base <= L->top && L->top < L->stack + L->stacksize); \
}

#define vmdispatch(o)	switch(o)
#define vmcase(l)	case l:
#define vmbreak		break


/*
** copy of 'luaV_gettable', but protecting the call to potential
** metamethod (which can reallocate the stack)
*/
// 传入表t和键k,将查询结果写入v
#define gettableProtected(L,t,k,v)  { const TValue *slot; \
  if (luaV_fastget(L,t,k,slot,luaH_get)) { setobj2s(L, v, slot); } \
  else Protect(luaV_finishget(L,t,k,v,slot)); }


/* same for 'luaV_settable' */
#define settableProtected(L,t,k,v) { const TValue *slot; \
  if (!luaV_fastset(L,t,k,slot,luaH_get,v)) \
    Protect(luaV_finishset(L,t,k,v,slot)); }



void luaV_execute (lua_State *L) {
  CallInfo *ci = L->ci;
  LClosure *cl;
  TValue *k;
  StkId base;
  // 进入luaV_execute函数的CallInfo被标记CIST_FRESH
  // 用来识别当前CallInfo是不是进入时的那一个
  ci->callstatus |= CIST_FRESH;  /* fresh invocation of 'luaV_execute" */
 newframe:  /* reentry point when frame changes (call/return) */
  lua_assert(ci == L->ci);
  cl = clLvalue(ci->func);  /* local reference to function's closure */
  k = cl->p->k;  /* local reference to function's constant table */
  base = ci->u.l.base;  /* local copy of function's base */
  /* main loop of interpreter */
  for (;;) {
    Instruction i;
    StkId ra;
    // 获取当前指令i和参数A代表的寄存器位置ra,让ci->u.l.savedpc自增
    vmfetch();
    vmdispatch (GET_OPCODE(i)) {
      vmcase(OP_MOVE) {
        setobjs2s(L, ra, RB(i));
        vmbreak;
      }
      vmcase(OP_LOADK) {
        TValue *rb = k + GETARG_Bx(i);
        setobj2s(L, ra, rb);
        vmbreak;
      }
      // OP_LOADKX还需要一条OP_EXTRAARG指令进行配合
      vmcase(OP_LOADKX) {
        TValue *rb;
        lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_EXTRAARG);
        rb = k + GETARG_Ax(*ci->u.l.savedpc++);
        setobj2s(L, ra, rb);
        vmbreak;
      }
      vmcase(OP_LOADBOOL) {
        setbvalue(ra, GETARG_B(i));
        if (GETARG_C(i)) ci->u.l.savedpc++;  /* skip next instruction (if C) */
        vmbreak;
      }
      vmcase(OP_LOADNIL) {
        // 总共需要设置b+1次nil
        int b = GETARG_B(i);
        do {
          setnilvalue(ra++);
        } while (b--);
        vmbreak;
      }
      vmcase(OP_GETUPVAL) {
        int b = GETARG_B(i);
        setobj2s(L, ra, cl->upvals[b]->v);
        vmbreak;
      }
      vmcase(OP_GETTABUP) {
        TValue *upval = cl->upvals[GETARG_B(i)]->v;
        TValue *rc = RKC(i);
        gettableProtected(L, upval, rc, ra);
        vmbreak;
      }
      vmcase(OP_GETTABLE) {
        StkId rb = RB(i);
        TValue *rc = RKC(i);
        gettableProtected(L, rb, rc, ra);
        vmbreak;
      }
      vmcase(OP_SETTABUP) {
        TValue *upval = cl->upvals[GETARG_A(i)]->v;
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        settableProtected(L, upval, rb, rc);
        vmbreak;
      }
      vmcase(OP_SETUPVAL) {
        UpVal *uv = cl->upvals[GETARG_B(i)];
        setobj(L, uv->v, ra);
        luaC_upvalbarrier(L, uv);
        vmbreak;
      }
      vmcase(OP_SETTABLE) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        settableProtected(L, ra, rb, rc);
        vmbreak;
      }
      vmcase(OP_NEWTABLE) {
        int b = GETARG_B(i);
        int c = GETARG_C(i);
        Table *t = luaH_new(L);
        sethvalue(L, ra, t);
        if (b != 0 || c != 0)
          luaH_resize(L, t, luaO_fb2int(b), luaO_fb2int(c));
        checkGC(L, ra + 1);
        vmbreak;
      }
      vmcase(OP_SELF) {
        // 让B位置赋值到A
        // 查询B.C放到A+1位置
        const TValue *aux;
        StkId rb = RB(i);
        TValue *rc = RKC(i);
        TString *key = tsvalue(rc);  /* key must be a string */
        setobjs2s(L, ra + 1, rb);
        if (luaV_fastget(L, rb, key, aux, luaH_getstr)) {
          setobj2s(L, ra, aux);
        }
        else Protect(luaV_finishget(L, rb, rc, ra, aux));
        vmbreak;
      }
      vmcase(OP_ADD) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, intop(+, ib, ic));
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numadd(L, nb, nc));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_ADD)); }
        vmbreak;
      }
      vmcase(OP_SUB) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, intop(-, ib, ic));
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numsub(L, nb, nc));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SUB)); }
        vmbreak;
      }
      vmcase(OP_MUL) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, intop(*, ib, ic));
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_nummul(L, nb, nc));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_MUL)); }
        vmbreak;
      }
      vmcase(OP_DIV) {  /* float division (always with floats) */
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numdiv(L, nb, nc));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_DIV)); }
        vmbreak;
      }
      vmcase(OP_BAND) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, intop(&, ib, ic));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BAND)); }
        vmbreak;
      }
      vmcase(OP_BOR) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, intop(|, ib, ic));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BOR)); }
        vmbreak;
      }
      vmcase(OP_BXOR) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, intop(^, ib, ic));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_BXOR)); }
        vmbreak;
      }
      vmcase(OP_SHL) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, luaV_shiftl(ib, ic));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHL)); }
        vmbreak;
      }
      vmcase(OP_SHR) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Integer ib; lua_Integer ic;
        if (tointeger(rb, &ib) && tointeger(rc, &ic)) {
          setivalue(ra, luaV_shiftl(ib, -ic));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_SHR)); }
        vmbreak;
      }
      vmcase(OP_MOD) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, luaV_mod(L, ib, ic));
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          lua_Number m;
          luai_nummod(L, nb, nc, m);
          setfltvalue(ra, m);
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_MOD)); }
        vmbreak;
      }
      vmcase(OP_IDIV) {  /* floor division */
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (ttisinteger(rb) && ttisinteger(rc)) {
          lua_Integer ib = ivalue(rb); lua_Integer ic = ivalue(rc);
          setivalue(ra, luaV_div(L, ib, ic));
        }
        else if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numidiv(L, nb, nc));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_IDIV)); }
        vmbreak;
      }
      vmcase(OP_POW) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        lua_Number nb; lua_Number nc;
        if (tonumber(rb, &nb) && tonumber(rc, &nc)) {
          setfltvalue(ra, luai_numpow(L, nb, nc));
        }
        else { Protect(luaT_trybinTM(L, rb, rc, ra, TM_POW)); }
        vmbreak;
      }
      vmcase(OP_UNM) {
        TValue *rb = RB(i);
        lua_Number nb;
        if (ttisinteger(rb)) {
          lua_Integer ib = ivalue(rb);
          setivalue(ra, intop(-, 0, ib));
        }
        else if (tonumber(rb, &nb)) {
          setfltvalue(ra, luai_numunm(L, nb));
        }
        else {
          Protect(luaT_trybinTM(L, rb, rb, ra, TM_UNM));
        }
        vmbreak;
      }
      vmcase(OP_BNOT) {
        TValue *rb = RB(i);
        lua_Integer ib;
        if (tointeger(rb, &ib)) {
          setivalue(ra, intop(^, ~l_castS2U(0), ib));
        }
        else {
          Protect(luaT_trybinTM(L, rb, rb, ra, TM_BNOT));
        }
        vmbreak;
      }
      vmcase(OP_NOT) {
        TValue *rb = RB(i);
        int res = l_isfalse(rb);  /* next assignment may change this value */
        setbvalue(ra, res);
        vmbreak;
      }
      vmcase(OP_LEN) {
        Protect(luaV_objlen(L, ra, RB(i)));
        vmbreak;
      }
      vmcase(OP_CONCAT) {
        int b = GETARG_B(i);
        int c = GETARG_C(i);
        StkId rb;
        L->top = base + c + 1;  /* mark the end of concat operands */
        Protect(luaV_concat(L, c - b + 1));
        ra = RA(i);  /* 'luaV_concat' may invoke TMs and move the stack */
        rb = base + b;
        setobjs2s(L, ra, rb);
        checkGC(L, (ra >= rb ? ra + 1 : rb));
        L->top = ci->top;  /* restore top */
        vmbreak;
      }
      vmcase(OP_JMP) {
        dojump(ci, i, 0);
        vmbreak;
      }
      vmcase(OP_EQ) {
        TValue *rb = RKB(i);
        TValue *rc = RKC(i);
        Protect(
          if (luaV_equalobj(L, rb, rc) != GETARG_A(i))
            ci->u.l.savedpc++;
          else
            donextjump(ci);
        )
        vmbreak;
      }
      vmcase(OP_LT) {
        Protect(
          if (luaV_lessthan(L, RKB(i), RKC(i)) != GETARG_A(i))
            ci->u.l.savedpc++;
          else
            donextjump(ci);
        )
        vmbreak;
      }
      vmcase(OP_LE) {
        Protect(
          if (luaV_lessequal(L, RKB(i), RKC(i)) != GETARG_A(i))
            ci->u.l.savedpc++;
          else
            donextjump(ci);
        )
        vmbreak;
      }
      vmcase(OP_TEST) {
        if (GETARG_C(i) ? l_isfalse(ra) : !l_isfalse(ra))
            ci->u.l.savedpc++;
          else
          donextjump(ci);
        vmbreak;
      }
      vmcase(OP_TESTSET) {
        TValue *rb = RB(i);
        if (GETARG_C(i) ? l_isfalse(rb) : !l_isfalse(rb))
          ci->u.l.savedpc++;
        else {
          setobjs2s(L, ra, rb);
          donextjump(ci);
        }
        vmbreak;
      }
      vmcase(OP_CALL) {
        int b = GETARG_B(i);
        int nresults = GETARG_C(i) - 1;
        if (b != 0) L->top = ra+b;  /* else previous instruction set top */
        if (luaD_precall(L, ra, nresults)) {  /* C function? */
          if (nresults >= 0)
            L->top = ci->top;  /* adjust results */
          Protect((void)0);  /* update 'base' */
        }
        else {  /* Lua function */
          // 在上面luaD_precall中修改了L->ci的指向
          ci = L->ci;
          // 每次新调用一个lua函数就重新开始for循环
          goto newframe;  /* restart luaV_execute over new Lua function */
        }
        vmbreak;
      }
      vmcase(OP_TAILCALL) {
        int b = GETARG_B(i);
        if (b != 0) L->top = ra+b;  /* else previous instruction set top */
        lua_assert(GETARG_C(i) - 1 == LUA_MULTRET);
        if (luaD_precall(L, ra, LUA_MULTRET)) {  /* C function? */
          Protect((void)0);  /* update 'base' */
        }
        else {
          /* tail call: put called frame (n) in place of caller one (o) */
          CallInfo *nci = L->ci;  /* called frame */
          CallInfo *oci = nci->previous;  /* caller frame */
          StkId nfunc = nci->func;  /* called function */
          StkId ofunc = oci->func;  /* caller function */
          /* last stack slot filled by 'precall' */
          StkId lim = nci->u.l.base + getproto(nfunc)->numparams;
          int aux;
          /* close all upvalues from previous call */
          if (cl->p->sizep > 0) luaF_close(L, oci->u.l.base);
          /* move new frame into old one */
          for (aux = 0; nfunc + aux < lim; aux++)
            setobjs2s(L, ofunc + aux, nfunc + aux);
          oci->u.l.base = ofunc + (nci->u.l.base - nfunc);  /* correct base */
          oci->top = L->top = ofunc + (L->top - nfunc);  /* correct top */
          oci->u.l.savedpc = nci->u.l.savedpc;
          oci->callstatus |= CIST_TAIL;  /* function was tail called */
          ci = L->ci = oci;  /* remove new frame */
          lua_assert(L->top == oci->u.l.base + getproto(ofunc)->maxstacksize);
          goto newframe;  /* restart luaV_execute over new Lua function */
        }
        vmbreak;
      }
      vmcase(OP_RETURN) {
        int b = GETARG_B(i);
        if (cl->p->sizep > 0) luaF_close(L, base);
        // 返回值的个数是b-1,如果b是0 返回值就是从A到栈顶
        b = luaD_poscall(L, ci, ra, (b != 0 ? b - 1 : cast_int(L->top - ra)));
        // 只有第一次执行的ci标记了CIST_FRESH
        // 当前已经到了最外部函数,执行结束
        if (ci->callstatus & CIST_FRESH)  /* local 'ci' still from callee */
          return;  /* external invocation: return */
        else {  /* invocation via reentry: continue execution */
          // 设置ci为外部函数
          ci = L->ci;
          if (b) L->top = ci->top;
          lua_assert(isLua(ci));
          // 现在ci是外部函数
          // 因为刚从函数调用退出来,ci即将执行的指令的前一条必定是OP_CALL
          lua_assert(GET_OPCODE(*((ci)->u.l.savedpc - 1)) == OP_CALL);
          goto newframe;  /* restart luaV_execute over new Lua function */
        }
      }
      vmcase(OP_FORLOOP) {
        if (ttisinteger(ra)) {  /* integer loop? */
          lua_Integer step = ivalue(ra + 2);
          lua_Integer idx = intop(+, ivalue(ra), step); /* increment index */
          lua_Integer limit = ivalue(ra + 1);
          if ((0 < step) ? (idx <= limit) : (limit <= idx)) {
            ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
            chgivalue(ra, idx);  /* update internal index... */
            setivalue(ra + 3, idx);  /* ...and external index */
          }
        }
        else {  /* floating loop */
          lua_Number step = fltvalue(ra + 2);
          lua_Number idx = luai_numadd(L, fltvalue(ra), step); /* inc. index */
          lua_Number limit = fltvalue(ra + 1);
          if (luai_numlt(0, step) ? luai_numle(idx, limit)
                                  : luai_numle(limit, idx)) {
            ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
            chgfltvalue(ra, idx);  /* update internal index... */
            setfltvalue(ra + 3, idx);  /* ...and external index */
          }
        }
        vmbreak;
      }
      vmcase(OP_FORPREP) {
        TValue *init = ra;
        TValue *plimit = ra + 1;
        TValue *pstep = ra + 2;
        lua_Integer ilimit;
        int stopnow;
        if (ttisinteger(init) && ttisinteger(pstep) &&
            forlimit(plimit, &ilimit, ivalue(pstep), &stopnow)) {
          /* all values are integer */
          lua_Integer initv = (stopnow ? 0 : ivalue(init));
          setivalue(plimit, ilimit);
          setivalue(init, intop(-, initv, ivalue(pstep)));
        }
        else {  /* try making all values floats */
          lua_Number ninit; lua_Number nlimit; lua_Number nstep;
          if (!tonumber(plimit, &nlimit))
            luaG_runerror(L, "'for' limit must be a number");
          setfltvalue(plimit, nlimit);
          if (!tonumber(pstep, &nstep))
            luaG_runerror(L, "'for' step must be a number");
          setfltvalue(pstep, nstep);
          if (!tonumber(init, &ninit))
            luaG_runerror(L, "'for' initial value must be a number");
          setfltvalue(init, luai_numsub(L, ninit, nstep));
        }
        ci->u.l.savedpc += GETARG_sBx(i);
        vmbreak;
      }
      vmcase(OP_TFORCALL) {
        StkId cb = ra + 3;  /* call base */
        setobjs2s(L, cb+2, ra+2);
        setobjs2s(L, cb+1, ra+1);
        setobjs2s(L, cb, ra);
        L->top = cb + 3;  /* func. + 2 args (state and index) */
        Protect(luaD_call(L, cb, GETARG_C(i)));
        L->top = ci->top;
        i = *(ci->u.l.savedpc++);  /* go to next instruction */
        ra = RA(i);
        lua_assert(GET_OPCODE(i) == OP_TFORLOOP);
        goto l_tforloop;
      }
      vmcase(OP_TFORLOOP) {
        l_tforloop:
        if (!ttisnil(ra + 1)) {  /* continue loop? */
          setobjs2s(L, ra, ra + 1);  /* save control variable */
           ci->u.l.savedpc += GETARG_sBx(i);  /* jump back */
        }
        vmbreak;
      }
      vmcase(OP_SETLIST) {
        int n = GETARG_B(i);
        int c = GETARG_C(i);
        unsigned int last;
        Table *h;
        if (n == 0) n = cast_int(L->top - ra) - 1;
        if (c == 0) {
          lua_assert(GET_OPCODE(*ci->u.l.savedpc) == OP_EXTRAARG);
          c = GETARG_Ax(*ci->u.l.savedpc++);
        }
        h = hvalue(ra);
        last = ((c-1)*LFIELDS_PER_FLUSH) + n;
        if (last > h->sizearray)  /* needs more space? */
          luaH_resizearray(L, h, last);  /* preallocate it at once */
        for (; n > 0; n--) {
          TValue *val = ra+n;
          luaH_setint(L, h, last--, val);
          luaC_barrierback(L, h, val);
        }
        L->top = ci->top;  /* correct top (in case of previous open call) */
        vmbreak;
      }
      vmcase(OP_CLOSURE) {
        // 找到要创建的函数的原型
        Proto *p = cl->p->p[GETARG_Bx(i)];
        // 先查询缓存
        LClosure *ncl = getcached(p, cl->upvals, base);  /* cached closure */
        if (ncl == NULL)  /* no match? */
          // 没有缓存结果 生成函数闭包
          pushclosure(L, p, cl->upvals, base, ra);  /* create a new one */
        else
          // 有缓存结果 直接写入寄存器
          setclLvalue(L, ra, ncl);  /* push cashed closure */
        checkGC(L, ra + 1);
        vmbreak;
      }
      vmcase(OP_VARARG) {
        // B-1是读取的结果个数
        int b = GETARG_B(i) - 1;  /* required results */
        int j;
        // 计算出...代表的结果的个数,也就是调用函数的时候给...传递的参数个数
        int n = cast_int(base - ci->func) - cl->p->numparams - 1;
        if (n < 0)  /* less arguments than parameters? */
          n = 0;  /* no vararg arguments */
        // b<0也就是B传入的0,代表获取可变参数的所有结果
        if (b < 0) {  /* B == 0? */
          b = n;  /* get all var. arguments */
          Protect(luaD_checkstack(L, n));
          ra = RA(i);  /* previous call may change the stack */
          // 从A寄存器开始写入...的所有结果,并设置栈顶
          L->top = ra + n;
        }
        for (j = 0; j < b && j < n; j++)
          setobjs2s(L, ra + j, base - n + j);
        for (; j < b; j++)  /* complete required results with nil */
          setnilvalue(ra + j);
        vmbreak;
      }
      vmcase(OP_EXTRAARG) {
        lua_assert(0);
        vmbreak;
      }
    }
  }
}

/* }================================================================== */

