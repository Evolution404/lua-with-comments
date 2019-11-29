/*
** $Id: llex.c,v 2.96 2016/05/02 14:02:12 roberto Exp $
** Lexical Analyzer 词法分析
** See Copyright Notice in lua.h
*/

#define llex_c
#define LUA_CORE

#include "lprefix.h"


#include <locale.h>
#include <string.h>

#include "lua.h"

#include "lctype.h"
#include "ldebug.h"
#include "ldo.h"
#include "lgc.h"
#include "llex.h"
#include "lobject.h"
#include "lparser.h"
#include "lstate.h"
#include "lstring.h"
#include "ltable.h"
#include "lzio.h"



// 传入LexState对象 设置current字段为下一个字符
#define next(ls) (ls->current = zgetc(ls->z))



// 检查current字段是否是换行符
#define currIsNewline(ls)	(ls->current == '\n' || ls->current == '\r')


/* ORDER RESERVED */
// 与RESERVED对应的字符串类型名称
static const char *const luaX_tokens [] = {
    "and", "break", "do", "else", "elseif",
    "end", "false", "for", "function", "goto", "if",
    "in", "local", "nil", "not", "or", "repeat",
    "return", "then", "true", "until", "while",
    "//", "..", "...", "==", ">=", "<=", "~=",
    "<<", ">>", "::", "<eof>",
    "<number>", "<integer>", "<name>", "<string>"
};


// 将当前字符保存进缓存 并将下一个字符读入当前字符
#define save_and_next(ls) (save(ls, ls->current), next(ls))


static l_noret lexerror (LexState *ls, const char *msg, int token);


// 在ls的buff上保存字符c
// ls被操作的词法状态 c要保存的字符
static void save (LexState *ls, int c) {
  Mbuffer *b = ls->buff;
  if (luaZ_bufflen(b) + 1 > luaZ_sizebuffer(b)) {  // 缓存空间不足了
    size_t newsize;
    if (luaZ_sizebuffer(b) >= MAX_SIZE/2)
      lexerror(ls, "lexical element too long", 0);
    // 将缓存空间扩容为2倍
    newsize = luaZ_sizebuffer(b) * 2;
    luaZ_resizebuffer(ls->L, b, newsize);
  }
  // 将c存进缓存中
  b->buffer[luaZ_bufflen(b)++] = cast(char, c);
}


// 词法分析初始化
// 创建_ENV和所有保留字的字符串对象
void luaX_init (lua_State *L) {
  int i;
  TString *e = luaS_newliteral(L, LUA_ENV);  /* create env name */
  luaC_fix(L, obj2gco(e));  /* never collect this name */
  for (i=0; i<NUM_RESERVED; i++) {
    TString *ts = luaS_new(L, luaX_tokens[i]);
    luaC_fix(L, obj2gco(ts));  /* reserved words are never collected */
    // extra字段标记是第几个关键字从1开始 and为1 break为2 ...
    ts->extra = cast_byte(i+1);  /* reserved word */
  }
}


// 从int类型token转换成字符串
const char *luaX_token2str (LexState *ls, int token) {
  if (token < FIRST_RESERVED) {  /* single-byte symbols? 判断是否是保留字 */
    lua_assert(token == cast_uchar(token));
    return luaO_pushfstring(ls->L, "'%c'", token);
  }
  else {
    const char *s = luaX_tokens[token - FIRST_RESERVED];  // 从int转换成字符串
    if (token < TK_EOS)  /* fixed format (symbols and reserved words)? eof之前都是固定格式直接压入栈顶 */
      return luaO_pushfstring(ls->L, "'%s'", s);
    else  /* names, strings, and numerals eof和其之后还需要继续处理不压入栈顶 */
      return s;
  }
}


// 将int型token转换成字符串压入栈顶
static const char *txtToken (LexState *ls, int token) {
  switch (token) {
    // 不是固定格式的符号即eof和其之后的符号
    // 对于这些符号从缓存中读取他们的实际字符串
    case TK_NAME: case TK_STRING:
    case TK_FLT: case TK_INT:
      save(ls, '\0');
      return luaO_pushfstring(ls->L, "'%s'", luaZ_buffer(ls->buff));
    default:  // 这个符号的字符串都是固定的
      return luaX_token2str(ls, token);
  }
}


static l_noret lexerror (LexState *ls, const char *msg, int token) {
  msg = luaG_addinfo(ls->L, msg, ls->source, ls->linenumber);
  if (token)
    luaO_pushfstring(ls->L, "%s near %s", msg, txtToken(ls, token));
  luaD_throw(ls->L, LUA_ERRSYNTAX);
}


l_noret luaX_syntaxerror (LexState *ls, const char *msg) {
  lexerror(ls, msg, ls->t.token);
}


/*
** creates a new string and anchors it in scanner's table so that
** it will not be collected until the end of the compilation
** (by that time it should be anchored somewhere)
*/
// 创建TString对象
// 查询ls->h表 表中key存在str那么就复用 否则以str创建key 便于下次复用
TString *luaX_newstring (LexState *ls, const char *str, size_t l) {
  lua_State *L = ls->L;
  TValue *o;  /* entry for 'str' */
  TString *ts = luaS_newlstr(L, str, l);  /* create new string */
  setsvalue2s(L, L->top++, ts);  /* temporarily anchor it in stack */
  o = luaH_set(L, ls->h, L->top - 1);  // 查询ls->h以str为键对应的value
  if (ttisnil(o)) {  /* not in use yet? 还没有str这个key */
    /* boolean value does not need GC barrier;
       table has no metatable, so it does not need to invalidate cache */
    setbvalue(o, 1);  /* t[string] = true 创建表中str项 */
    luaC_checkGC(L);
  }
  else {  /* string already present */
    ts = tsvalue(keyfromval(o));  /* re-use value previously stored 复用之前的key */
  }
  L->top--;  /* remove string from stack */
  return ts;
}


/*
** increment line number and skips newline sequence (any of
** \n, \r, \n\r, or \r\n)
*/
// 跳过ls->current现在所指的换行符 增加行号
static void inclinenumber (LexState *ls) {
  int old = ls->current;
  lua_assert(currIsNewline(ls));  // 确保ls->current是换行符
  next(ls);  /* skip '\n' or '\r' 跳过换行符 */
  if (currIsNewline(ls) && ls->current != old)  // 不同平台换行符不一样 换行符有可能有两个字符
    next(ls);  /* skip '\n\r' or '\r\n' */
  if (++ls->linenumber >= MAX_INT)
    lexerror(ls, "chunk has too many lines", 0);
}


// 为LexState设置输入文件 并设置初始状态
void luaX_setinput (lua_State *L, LexState *ls, ZIO *z, TString *source,
                    int firstchar) {
  ls->t.token = 0;
  ls->L = L;
  ls->current = firstchar;
  ls->lookahead.token = TK_EOS;  /* no look-ahead token */
  ls->z = z;
  ls->fs = NULL;
  ls->linenumber = 1;
  ls->lastline = 1;
  ls->source = source;
  ls->envn = luaS_newliteral(L, LUA_ENV);  /* get env name */
  luaZ_resizebuffer(ls->L, ls->buff, LUA_MINBUFFER);  /* initialize buffer */
}



/*
** =======================================================
** LEXICAL ANALYZER
** =======================================================
*/


// 查看当前字符是不是c
// 返回1 当前字符是c并跳过当前字符
// 返回0 当前字符不是c 不做其他操作
static int check_next1 (LexState *ls, int c) {
  if (ls->current == c) {
    next(ls);
    return 1;
  }
  else return 0;
}


/*
** Check whether current char is in set 'set' (with two chars) and
** saves it
*/
// set是一个长度为2的字符串 查看当前字符是不是set中两个字符的其中一个
// 返回1 当前字符在set中 保存当前字符并跳过当前字符
// 返回0 当前字符不在set中 不做其他操作
static int check_next2 (LexState *ls, const char *set) {
  lua_assert(set[2] == '\0');  // set字符串一定只有两个字符
  if (ls->current == set[0] || ls->current == set[1]) {
    save_and_next(ls);
    return 1;
  }
  else return 0;
}


/* LUA_NUMBER */
/*
** this function is quite liberal in what it accepts, as 'luaO_str2num'
** will reject ill-formed numerals.
*/
// 处理数字符号 识别的第一位一定是数字不能是负号
// 返回值表示识别数字的类型 整数或浮点数 seminfo存储识别后的数字值
static int read_numeral (LexState *ls, SemInfo *seminfo) {
  TValue obj;
  const char *expo = "Ee";  // 指数部分的标记 ex代表10^x
  int first = ls->current;
  lua_assert(lisdigit(ls->current));
  save_and_next(ls);
  // 检查是不是16进制数
  if (first == '0' && check_next2(ls, "xX"))  /* hexadecimal? 前两位是0x */
    expo = "Pp";  // px代表2^x 0xfp1 就是15*2^1=30
  for (;;) {  // 识别 数字,小数点,指数
    if (check_next2(ls, expo))  /* exponent part? */
      check_next2(ls, "-+");  /* optional exponent sign */
    if (lisxdigit(ls->current))
      save_and_next(ls);
    else if (ls->current == '.')
      save_and_next(ls);
    else break;
  }
  save(ls, '\0');
  if (luaO_str2num(luaZ_buffer(ls->buff), &obj) == 0)  /* format error? */
    lexerror(ls, "malformed number", TK_FLT);
  if (ttisinteger(&obj)) {
    seminfo->i = ivalue(&obj);
    return TK_INT;
  }
  else {
    lua_assert(ttisfloat(&obj));
    seminfo->r = fltvalue(&obj);
    return TK_FLT;
  }
}


/*
** skip a sequence '[=*[' or ']=*]'; if sequence is well formed, return
** its number of '='s; otherwise, return a negative number (-1 iff there
** are no '='s after initial bracket)
*/
// 识别多行字符串的开始或结束符号 执行结束current指向第一个不是=的符号
// 返回识别到的=的个数 如果识别不匹配返回 -1-识别=个数
static int skip_sep (LexState *ls) {
  int count = 0;
  int s = ls->current;
  lua_assert(s == '[' || s == ']');
  save_and_next(ls);
  while (ls->current == '=') {
    save_and_next(ls);
    count++;
  }
  return (ls->current == s) ? count : (-count) - 1;
}


// 该函数用于识别多行注释或多行字符串
// lua多行字符串的格式
// [ 中间可放任意个=号 [
// 多行文字
// ] 结束时使用相同数量的=号 ]
// 多行注释格式
// --[ 中间可放任意个=号 [
// 多行文字
// ] 结束时使用相同数量的=号 ]
// seminfo为NULL说明识别注释 不是NULL说明在识别多行字符串
// 开始执行是current指向[ 是注释或多行字符串的开始部分的第二个[
// 执行结束current指向结构的第二个]的下一个
static void read_long_string (LexState *ls, SemInfo *seminfo, int sep) {
  int line = ls->linenumber;  /* initial line (for error message) */
  save_and_next(ls);  /* skip 2nd '[' */
  // 现在进入注释或多行字符串的实际内容
  if (currIsNewline(ls))  /* string starts with a newline? 开始就换行了 */
    inclinenumber(ls);  /* skip it 开始的换行符不需要 */
  for (;;) {
    switch (ls->current) {
      case EOZ: {  /* error */
        const char *what = (seminfo ? "string" : "comment");
        const char *msg = luaO_pushfstring(ls->L,
                     "unfinished long %s (starting at line %d)", what, line);
        lexerror(ls, msg, TK_EOS);
        break;  /* to avoid warnings */
      }
      case ']': {  // 识别到结束的位置
        if (skip_sep(ls) == sep) {
          save_and_next(ls);  /* skip 2nd ']' 跳过结束位置的第二个] */
          goto endloop;
        }
        break;
      }
      case '\n': case '\r': {
        save(ls, '\n');
        inclinenumber(ls);
        if (!seminfo) luaZ_resetbuffer(ls->buff);  /* avoid wasting space 注释没必要浪费空间保存 */
        break;
      }
      default: {  // 在识别实际的内容
        if (seminfo) save_and_next(ls);
        else next(ls);
      }
    }
  } endloop:
  if (seminfo)
    // 此时缓存里是[=*[xxx]=*] xxx是实际字符串
    // 字符串的开始位置是ls->buff+等号个数+2  2是两个[
    // 字符串长度显然就是缓存的大小去掉开始结尾的部分
    seminfo->ts = luaX_newstring(ls, luaZ_buffer(ls->buff) + (2 + sep),
                                     luaZ_bufflen(ls->buff) - 2*(2 + sep));
}


// c是一个布尔值
// c为真不做任何操作
// c为假那么就 缓存current并指向下一个 输出错误信息msg
static void esccheck (LexState *ls, int c, const char *msg) {
  if (!c) {
    if (ls->current != EOZ)
      save_and_next(ls);  /* add current to buffer for error message */
    lexerror(ls, msg, TK_STRING);
  }
}


// 读取current后的一个16进制数
// 保存当前current current后移一个 返回16进制转换成10进制结果 如a为10
static int gethexa (LexState *ls) {
  save_and_next(ls);
  esccheck (ls, lisxdigit(ls->current), "hexadecimal digit expected");
  return luaO_hexavalue(ls->current);
}


// 读取两个16进制数 current后移两个
// 返回两位16进制数的10进制结果 缓存中不保存
static int readhexaesc (LexState *ls) {
  int r = gethexa(ls);
  r = (r << 4) + gethexa(ls);  // 16进制数占用4位 r现在是两位的16进制数
  luaZ_buffremove(ls->buff, 2);  /* remove saved chars from buffer */
  return r;
}


// 读取一个utf8值 返回该utf8的10进制值
// 格式为\u{ffff}  ffff代表n个16进制数 如lua交互窗口输入 '\u{607c}' 打印 恼
// 执行结束current指向}的后一个
static unsigned long readutf8esc (LexState *ls) {
  unsigned long r;
  int i = 4;  /* chars to be removed: '\', 'u', '{', and first digit */
  save_and_next(ls);  /* skip 'u' */
  esccheck(ls, ls->current == '{', "missing '{'");
  // 此时缓存 \u{
  r = gethexa(ls);  /* must have at least one digit */
  while ((save_and_next(ls), lisxdigit(ls->current))) {
    i++;
    r = (r << 4) + luaO_hexavalue(ls->current);  // 计算16进制值
    esccheck(ls, r <= 0x10FFFF, "UTF-8 value too large");
  }
  // 此时缓存 \u{ffff  ffff代表n个16进制数
  esccheck(ls, ls->current == '}', "missing '}'");
  next(ls);  /* skip '}' 没有保存另一半大括号} */
  luaZ_buffremove(ls->buff, i);  /* remove saved chars from buffer 清空之前保存的值 */
  return r;
}


// 处理utf8字符 \u{ffff} 转换成utf8编码值并保存到缓存中
// 执行current在}后一个字符
static void utf8esc (LexState *ls) {
  char buff[UTF8BUFFSZ];
  int n = luaO_utf8esc(buff, readutf8esc(ls));
  for (; n > 0; n--)  /* add 'buff' to string */
    save(ls, buff[UTF8BUFFSZ - n]);
}


// 读取三位十进制数 转换成int返回 current指向三位数字下一个
static int readdecesc (LexState *ls) {
  int i;
  int r = 0;  /* result accumulator */
  for (i = 0; i < 3 && lisdigit(ls->current); i++) {  /* read up to 3 digits */
    r = 10*r + ls->current - '0';  // 计算10进制值
    save_and_next(ls);
  }
  esccheck(ls, r <= UCHAR_MAX, "decimal escape too large");
  luaZ_buffremove(ls->buff, i);  /* remove read digits from buffer */
  return r;
}


// del是"或' 即del是引号
// 读取一个被del包围的字符串 结束后current指向结束引号的下一个
static void read_string (LexState *ls, int del, SemInfo *seminfo) {
  // 开始执行current指向"或'
  save_and_next(ls);  /* keep delimiter (for error messages) 保存引号 */
  while (ls->current != del) {  // 遍历引号内部的内容
    switch (ls->current) {
      case EOZ:
        lexerror(ls, "unfinished string", TK_EOS);
        break;  /* to avoid warnings */
      case '\n':
      case '\r':
        lexerror(ls, "unfinished string", TK_STRING);
        break;  /* to avoid warnings */
      case '\\': {  /* escape sequences 读到反斜杠进行特殊处理 */
        int c;  /* final character to be saved */
        save_and_next(ls);  /* keep '\\' for error messages 预先保留一个反斜杠 */
        switch (ls->current) {
          case 'a': c = '\a'; goto read_save;
          case 'b': c = '\b'; goto read_save;
          case 'f': c = '\f'; goto read_save;
          case 'n': c = '\n'; goto read_save;
          case 'r': c = '\r'; goto read_save;
          case 't': c = '\t'; goto read_save;
          case 'v': c = '\v'; goto read_save;
          case 'x': c = readhexaesc(ls); goto read_save;  // 例如 \x50 c被转换成80是P(大写p)
          case 'u': utf8esc(ls);  goto no_save;  // 读取一个utf8字符 utf8esc已经保存了不需要重新保存
          case '\n': case '\r':
            inclinenumber(ls); c = '\n'; goto only_save;  // 单行字符串内部出现了换行
          case '\\': case '\"': case '\'':  // \\ \" \'都是为了转换成它们自己
            c = ls->current; goto read_save;
          case EOZ: goto no_save;  /* will raise an error next loop */
          case 'z': {  /* zap following span of spaces \z可以省略掉它后面的空白符 */
            luaZ_buffremove(ls->buff, 1);  /* remove '\\' */
            next(ls);  /* skip the 'z' */
            while (lisspace(ls->current)) {
              if (currIsNewline(ls)) inclinenumber(ls);
              else next(ls);
            }
            goto no_save;
          }
          default: {  // 处理\ddd(d是数字)
            esccheck(ls, lisdigit(ls->current), "invalid escape sequence");
            c = readdecesc(ls);  /* digital escape '\ddd' */
            goto only_save;
          }
        }
       read_save:  // 读取下一个字符并保存当前读到的字符c
         next(ls);
         /* go through */
       only_save:  // 不读取下一个字符只保存当前字符c
         luaZ_buffremove(ls->buff, 1);  /* remove '\\' 删掉预先保存的反斜杠 */
         save(ls, c);  // 保存当前字符c
         /* go through */
       no_save: break;  // 什么都不做
      }
      default:  // 正常的字符串内部的字符直接保存就行了
        save_and_next(ls);
    }
  }
  // 现在current指向引号
  save_and_next(ls);  /* skip delimiter */
  // current指向引号下一个
  // 保存缓存中存放的字符串 去掉开头结尾的引号
  seminfo->ts = luaX_newstring(ls, luaZ_buffer(ls->buff) + 1,
                                   luaZ_bufflen(ls->buff) - 2);
}


// 读取一个符号 结束后current指向识别符号后的下一个字符
// 返回值小于257 识别到一个ascii字符 识别结果就是返回值代表的字符
// 返回值大于256 识别到一个RESERVED中定义的符号 返回值代表符号类型 如果识别出具体的值存储在seminfo中
static int llex (LexState *ls, SemInfo *seminfo) {
  luaZ_resetbuffer(ls->buff);  // 清空缓存
  for (;;) {
    switch (ls->current) {  // 根据当前的字符进行不同处理
      case '\n': case '\r': {  /* line breaks 识别到换行 */
        inclinenumber(ls);
        break;
      }
      case ' ': case '\f': case '\t': case '\v': {  /* spaces \f换页 \v垂直制表符 */
        next(ls);  // 空白符号直接跳过
        break;
      }
      case '-': {  /* '-' or '--' (comment) */
        next(ls);
        if (ls->current != '-') return '-';  // 单独的减号就是减号
        /* else is a comment 识别到两个减号说明是注释 */
        next(ls);
        if (ls->current == '[') {  /* long comment? 此时在多行注释的第一个[ */
          // 处理多行注释
          int sep = skip_sep(ls);
          // 此时在多行注释的第二个[
          luaZ_resetbuffer(ls->buff);  /* 'skip_sep' may dirty the buffer 注释不需要保存内容 */
          if (sep >= 0) {
            read_long_string(ls, NULL, sep);  /* skip long comment 跳过整个注释 current指向第二个]的下一个 */
            luaZ_resetbuffer(ls->buff);  /* previous call may dirty the buff. */
            break;
          }
        }
        /* else short comment 处理单行注释 */
        while (!currIsNewline(ls) && ls->current != EOZ)  // 跳过这一行或者到了文件尾
          next(ls);  /* skip until end of line (or end of file) */
        break;
      }
      case '[': {  /* long string or simply '[' 可能是长字符串或者只是[ */
        int sep = skip_sep(ls);
        // 此时current指向第二个[ 缓存里是[=* 即[和n个等号
        if (sep >= 0) {
          read_long_string(ls, seminfo, sep);  // 读取字符串
          return TK_STRING;
        }
        else if (sep != -1)  /* '[=...' missing second bracket 不是-1说明是[=*这种 */
          lexerror(ls, "invalid long string delimiter", TK_STRING);
        return '[';  // sep是-1 说明只是单纯的一个[
      }
      case '=': {
        next(ls);
        if (check_next1(ls, '=')) return TK_EQ;  // ==
        else return '=';  // 只有一个=
      }
      case '<': {
        next(ls);
        if (check_next1(ls, '=')) return TK_LE;  // <=
        else if (check_next1(ls, '<')) return TK_SHL;  // <<
        else return '<';  // <
      }
      case '>': {  // 和上面<一样
        next(ls);
        if (check_next1(ls, '=')) return TK_GE;
        else if (check_next1(ls, '>')) return TK_SHR;
        else return '>';
      }
      case '/': {
        next(ls);
        if (check_next1(ls, '/')) return TK_IDIV;  // 识别到//
        else return '/';  // 只是/
      }
      case '~': {
        next(ls);
        if (check_next1(ls, '=')) return TK_NE;  // ~=
        else return '~';  // ~
      }
      case ':': {
        next(ls);
        if (check_next1(ls, ':')) return TK_DBCOLON;  // ::
        else return ':';  // :
      }
      case '"': case '\'': {  /* short literal strings 单行字符串 */
        read_string(ls, ls->current, seminfo);  // 读取字符串并保存在seminfo中
        return TK_STRING;
      }
      case '.': {  /* '.', '..', '...', or number */
        save_and_next(ls);
        if (check_next1(ls, '.')) {
          if (check_next1(ls, '.'))
            return TK_DOTS;   /* '...' */
          else return TK_CONCAT;   /* '..' */
        }
        else if (!lisdigit(ls->current)) return '.';
        else return read_numeral(ls, seminfo);  // current指向一个数字
      }
      case '0': case '1': case '2': case '3': case '4':  // 识别数字
      case '5': case '6': case '7': case '8': case '9': {
        return read_numeral(ls, seminfo);
      }
      case EOZ: {  // 文件结束
        return TK_EOS;
      }
      default: {
        if (lislalpha(ls->current)) {  /* identifier or reserved word? */
          // 第一位是字母或下划线之后位的是数字字母下划线
          TString *ts;
          do {
            save_and_next(ls);
          } while (lislalnum(ls->current));
          ts = luaX_newstring(ls, luaZ_buffer(ls->buff),
                                  luaZ_bufflen(ls->buff));
          seminfo->ts = ts;
          if (isreserved(ts))  /* reserved word? 识别出来的是关键字 */
            return ts->extra - 1 + FIRST_RESERVED;  // 关键字的extra从1开始
          else {
            return TK_NAME;  // 不是关键字是一个变量名
          }
        }
        else {  /* single-char tokens (+ - / ...) 一个单独的字符直接返回 */
          int c = ls->current;
          next(ls);
          return c;
        }
      }
    }
  }
}


// 向后读取一个符号存放在ls->t中
// 先查看lookahead 如果有直接将它放到ls->t中 没有则读取一个
void luaX_next (LexState *ls) {
  ls->lastline = ls->linenumber;
  if (ls->lookahead.token != TK_EOS) {  /* is there a look-ahead token? 有向前看的信息直接读取它 */
    ls->t = ls->lookahead;  /* use this one */
    ls->lookahead.token = TK_EOS;  /* and discharge it */
  }
  else  // 没有向前看的信息
    ls->t.token = llex(ls, &ls->t.seminfo);  /* read next token 读取一个符号 */
}


// 读取一个符号到lookahead中 返回读取到的符号类型
// 调用时ls必须没有向前看信息
int luaX_lookahead (LexState *ls) {
  lua_assert(ls->lookahead.token == TK_EOS);  // 确保没有向前看的信息
  ls->lookahead.token = llex(ls, &ls->lookahead.seminfo);  // 读一个符号到lookahead中
  return ls->lookahead.token;  // 返回读到的符号类型
}

