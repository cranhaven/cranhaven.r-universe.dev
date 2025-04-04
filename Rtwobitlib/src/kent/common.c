/* Commonly used routines in a wide range of applications.
 * Strings, singly-linked lists, and a little file i/o.
 *
 * This file is copyright 2002 Jim Kent, but license is hereby
 * granted for all use - public, private or commercial. */

#include "common.h"
#include "errAbort.h"
#include "linefile.h"

void *cloneMem(void *pt, size_t size)
/* Allocate a new buffer of given size, and copy pt to it. */
{
void *newPt = needLargeMem(size);
memcpy(newPt, pt, size);
return newPt;
}

static char *cloneStringZExt(const char *s, int size, int copySize)
/* Make a zero terminated copy of string in memory */
{
char *d = needMem(copySize+1);
copySize = min(size,copySize);
memcpy(d, s, copySize);
d[copySize] = 0;
return d;
}

char *cloneStringZ(const char *s, int size)
/* Make a zero terminated copy of string in memory */
{
return cloneStringZExt(s, strlen(s), size);
}

char *cloneString(const char *s)
/* Make copy of string in dynamic memory */
{
int size = 0;
if (s == NULL)
    return NULL;
size = strlen(s);
return cloneStringZExt(s, size, size);
}

/* Reverse the order of the bytes. */
void reverseBytes(char *bytes, long length)
{
long halfLen = (length>>1);
char *end = bytes+length;
char c;
while (--halfLen >= 0)
    {
    c = *bytes;
    *bytes++ = *--end;
    *end = c;
    }
}

void reverseInts(int *a, int length)
/* Reverse the order of the integer array. */
{
int halfLen = (length>>1);
int *end = a+length;
int c;
while (--halfLen >= 0)
    {
    c = *a;
    *a++ = *--end;
    *end = c;
    }
}

void reverseUnsigned(unsigned *a, int length)
/* Reverse the order of the unsigned array. */
{
int halfLen = (length>>1);
unsigned *end = a+length;
unsigned c;
while (--halfLen >= 0)
    {
    c = *a;
    *a++ = *--end;
    *end = c;
    }
}

void reverseDoubles(double *a, int length)
/* Reverse the order of the double array. */
{
int halfLen = (length>>1);
double *end = a+length;
double c;
while (--halfLen >= 0)
    {
    c = *a;
    *a++ = *--end;
    *end = c;
    }
}

void reverseStrings(char **a, int length)
/* Reverse the order of the char* array. */
{
int halfLen = (length>>1);
char **end = a+length;
char *c;
while (--halfLen >= 0)
    {
    c = *a;
    *a++ = *--end;
    *end = c;
    }
}

/** List managing routines. */

/* Count up elements in list. */
int slCount(const void *list)
{
struct slList *pt = (struct slList *)list;
int len = 0;

while (pt != NULL)
    {
    len += 1;
    pt = pt->next;
    }
return len;
}

void *slLastEl(void *list)
/* Returns last element in list or NULL if none. */
{
struct slList *next, *el;
if ((el = list) == NULL)
    return NULL;
while ((next = el->next) != NULL)
    el = next;
return el;
}

/* Add new node to tail of list.
 * Usage:
 *    slAddTail(&list, node);
 * where list and nodes are both pointers to structure
 * that begin with a next pointer.
 */
void slAddTail(void *listPt, void *node)
{
struct slList **ppt = (struct slList **)listPt;
struct slList *n = (struct slList *)node;

while (*ppt != NULL)
    {
    ppt = &((*ppt)->next);
    }
n->next = NULL;
*ppt = n;
}

void *slPopHead(void *vListPt)
/* Return head of list and remove it from list. (Fast) */
{
struct slList **listPt = (struct slList **)vListPt;
struct slList *el = *listPt;
if (el != NULL)
    {
    *listPt = el->next;
    el->next = NULL;
    }
return el;
}

void *slPopTail(void *vListPt)
/* Return tail of list and remove it from list. (Not so fast) */
{
struct slList **listPt = (struct slList **)vListPt;
struct slList *el = *listPt;
if (el != NULL)
    {
    for (;;)
        {
        if (el->next == NULL)
            {
            *listPt = NULL;
            break;
            }
        listPt = &el->next;
        el = el->next;
        }
    }
return el;
}



void *slCat(void *va, void *vb)
/* Return concatenation of lists a and b.
 * Example Usage:
 *   struct slName *a = getNames("a");
 *   struct slName *b = getNames("b");
 *   struct slName *ab = slCat(a,b)
 */
{
struct slList *a = va;
struct slList *b = vb;
struct slList *end;
if (a == NULL)
    return b;
for (end = a; end->next != NULL; end = end->next)
    ;
end->next = b;
return a;
}

void slReverse(void *listPt)
/* Reverse order of a list.
 * Usage:
 *    slReverse(&list);
 */
{
struct slList **ppt = (struct slList **)listPt;
struct slList *newList = NULL;
struct slList *el, *next;

next = *ppt;
while (next != NULL)
    {
    el = next;
    next = el->next;
    el->next = newList;
    newList = el;
    }
*ppt = newList;
}

void slFreeList(void *listPt)
/* Free list */
{
struct slList **ppt = (struct slList**)listPt;
struct slList *next = *ppt;
struct slList *el;

while (next != NULL)
    {
    el = next;
    next = el->next;
    freeMem((char*)el);
    }
*ppt = NULL;
}

void slSort(void *pList, int (*compare )(const void *elem1,  const void *elem2))
/* Sort a singly linked list with Qsort and a temporary array. */
{
struct slList **pL = (struct slList **)pList;
struct slList *list = *pL;
int count;
count = slCount(list);
if (count > 1)
    {
    struct slList *el;
    struct slList **array;
    int i;
    array = needLargeMem(count * sizeof(*array));
    for (el = list, i=0; el != NULL; el = el->next, i++)
        array[i] = el;
    qsort(array, count, sizeof(array[0]), compare);
    list = NULL;
    for (i=0; i<count; ++i)
        {
        array[i]->next = list;
        list = array[i];
        }
    freeMem(array);
    slReverse(&list);
    *pL = list;
    }
}

boolean slRemoveEl(void *vpList, void *vToRemove)
/* Remove element from singly linked list.  Usage:
 *    slRemove(&list, el);
 * Returns TRUE if element in list.  */
{
struct slList **pList = vpList;
struct slList *toRemove = vToRemove;
struct slList *el, *next, *newList = NULL;
boolean didRemove = FALSE;

for (el = *pList; el != NULL; el = next)
    {
    next = el->next;
    if (el != toRemove)
	{
	slAddHead(&newList, el);
	}
    else
        didRemove = TRUE;
    }
slReverse(&newList);
*pList = newList;
return didRemove;
}

struct slName *newSlName(char *name)
/* Return a new name. */
{
struct slName *sn;
if (name != NULL)
    {
    int len = strlen(name);
    sn = needMem(sizeof(*sn)+len);
    strcpy(sn->name, name);
    return sn;
    }
else
    {
    AllocVar(sn);
    }
return sn;
}

struct slRef *refOnList(struct slRef *refList, void *val)
/* Return ref if val is already on list, otherwise NULL. */
{
struct slRef *ref;
for (ref = refList; ref != NULL; ref = ref->next)
    if (ref->val == val)
        return ref;
return NULL;
}

struct slRef *slRefNew(void *val)
/* Create new slRef element. */
{
struct slRef *ref;
AllocVar(ref);
ref->val = val;
return ref;
}

void refAdd(struct slRef **pRefList, void *val)
/* Add reference to list. */
{
struct slRef *ref;
AllocVar(ref);
ref->val = val;
slAddHead(pRefList, ref);
}

struct slPair *slPairNew(char *name, void *val)
/* Allocate new name/value pair. */
{
struct slPair *el;
AllocVar(el);
el->name = cloneString(name);
el->val = val;
return el;
}

void slPairAdd(struct slPair **pList, char *name, void *val)
/* Add new slPair to head of list. */
{
struct slPair *el = slPairNew(name, val);
slAddHead(pList, el);
}

void slPairFree(struct slPair **pEl)
/* Free up struct and name.  (Don't free up values.) */
{
struct slPair *el = *pEl;
if (el != NULL)
    {
    freeMem(el->name);
    freez(pEl);
    }
}

void slPairFreeList(struct slPair **pList)
/* Free up list.  (Don't free up values.) */
{
struct slPair *el, *next;

for (el = *pList; el != NULL; el = next)
    {
    next = el->next;
    slPairFree(&el);
    }
*pList = NULL;
}

struct slPair *slPairFind(struct slPair *list, char *name)
/* Return list element of given name, or NULL if not found. */
{
struct slPair *el;
for (el = list; el != NULL; el = el->next)
    if (sameString(name, el->name))
        break;
return el;
}

void *slPairFindVal(struct slPair *list, char *name)
/* Return value associated with name in list, or NULL if not found. */
{
struct slPair *el = slPairFind(list, name);
if (el == NULL)
    return NULL;
return el->val;
}

struct slPair *slPairListFromString(const char *str,boolean respectQuotes)
// Return slPair list parsed from list in string like:  [name1=val1 name2=val2 ...]
// if respectQuotes then string can have double quotes: [name1="val 1" "name 2"=val2 ...]
//    resulting pair strips quotes: {name1}={val 1},{name 2}={val2}
// Returns NULL if parse error.  Free this up with slPairFreeValsAndList.
{
char *s = skipLeadingSpaces(str);  // Would like to remove this and tighten up the standard someday.
if (isEmpty(s))
    return NULL;

struct slPair *list = NULL;
char name[1024];
char val[1024];
char buf[1024];
bool inQuote = FALSE;
char *b = buf;
char sep = '=';
char c = ' ';
int mode = 0;
while(1)
    {
    c = *s++;
    if (mode == 0 || mode == 2) // reading name or val
	{
	boolean term = FALSE;
	if (respectQuotes && b == buf && !inQuote && c == '"')
	    inQuote = TRUE;
	else if (inQuote && c == '"')
	    term = TRUE;
	else if ((c == sep || c == 0) && !inQuote)
	    {
	    term = TRUE;
	    --s;  // rewind
	    }
	else if (c == ' ' && !inQuote)
	    {
	    warn("slPairListFromString: Unexpected whitespace in %s", str);
	    return NULL;
	    }
	else if (c == 0 && inQuote)
	    {
	    warn("slPairListFromString: Unterminated quote in %s", str);
	    return NULL;
	    }
	else
	    {
	    *b++ = c;
	    if ((b - buf) > sizeof buf)
		{
		warn("slPairListFromString: pair name or value too long in %s", str);
		return NULL;
		}
	    }
	if (term)
	    {
	    inQuote = FALSE;
	    *b = 0;
	    if (mode == 0)
		{
		safecpy(name, sizeof name, buf);
		if (strlen(name)<1)
		    {
		    warn("slPairListFromString: Pair name cannot be empty in %s", str);
		    return NULL;
		    }
		// Shall we check for name being alphanumeric, at least for the respectQuotes=FALSE case?
		}
	    else // mode == 2
                {
		safecpy(val, sizeof val, buf);
		if (!respectQuotes && (hasWhiteSpace(name) || hasWhiteSpace(val))) // should never happen
		    {
		    warn("slPairListFromString() Unexpected white space in name=value pair: [%s]=[%s] in string=[%s]\n", name, val, str);
		    break;
		    }
		slPairAdd(&list, name, cloneString(val));
		}
	    ++mode;
	    }
	}
    else if (mode == 1) // read required "=" sign
	{
	if (c != '=')
	    {
	    warn("slPairListFromString: Expected character = after name in %s", str);
	    return NULL;
            }
	++mode;
	sep = ' ';
	b = buf;
	}
    else // (mode == 3) reading optional separating space
	{
	if (c == 0)
	    break;
	if (c != ' ')
	    {
	    mode = 0;
	    --s;
	    b = buf;
	    sep = '=';
	    }
	}
    }
slReverse(&list);
return list;
}

char *slPairListToString(struct slPair *list,boolean quoteIfSpaces)
// Returns an allocated string of pairs in form of [name1=val1 name2=val2 ...]
// If requested, will wrap name or val in quotes if contain spaces: [name1="val 1" "name 2"=val2]
{
// Don't rely on dyString.  We should do the accounting ourselves and not create extra dependencies.
int count = 0;
struct slPair *pair = list;
for (;pair != NULL; pair = pair->next)
    {
    assert(pair->name != NULL && pair->val != NULL); // Better assert and get this over with,
                                                     // complete with stack
    count += strlen(pair->name);
    count += strlen((char *)(pair->val));
    count += 2; // = and ' ' delimit
    if (quoteIfSpaces)
        {
        if (hasWhiteSpace(pair->name))
            count += 2; // " and "
        if (hasWhiteSpace((char *)(pair->val)))
            count += 2; // " and "
        }
    }
if (count == 0)
    return NULL;

char *str = needMem(count+5); // A bit of slop

char *strPtr = str;
for (pair = list; pair != NULL; pair = pair->next, strPtr += strlen(strPtr))
    {
    if (pair != list) // Not first cycle
        *strPtr++ = ' ';
    if (hasWhiteSpace(pair->name))
        {
        if (quoteIfSpaces)
            snprintf(strPtr,(count+4)-(strPtr-str),"\"%s\"=",pair->name);
        else
            {
            warn("slPairListToString() Unexpected white space in name: [%s]\n", pair->name);
            snprintf(strPtr,(count+4)-(strPtr-str),"%s=",pair->name); // warn but still make string
            }
        }
    else
        snprintf(strPtr,(count+4)-(strPtr-str),"%s=",pair->name);
    strPtr += strlen(strPtr);
    if (hasWhiteSpace((char *)(pair->val)))
        {
        if (quoteIfSpaces)
            snprintf(strPtr,(count+4)-(strPtr-str),"\"%s\"",(char *)(pair->val));
        else
            {
            warn("slPairListToString() Unexpected white space in val: [%s]\n", (char *)(pair->val));
            snprintf(strPtr,(count+4)-(strPtr-str),"%s",(char *)(pair->val)); // warn but still make string
            }
        }
    else
        snprintf(strPtr,(count+4)-(strPtr-str),"%s",(char *)(pair->val));
    }
return str;
}

char *slPairNameToString(struct slPair *list, char delimiter,boolean quoteIfSpaces)
// Return string created by joining all names (ignoring vals) with the delimiter.
// If requested, will wrap name in quotes if contain spaces: [name1,"name 2" ...]
{
int elCount = 0;
int count = 0;
struct slPair *pair = list;
for (; pair != NULL; pair = pair->next, elCount++)
    {
    assert(pair->name != NULL);
    count += strlen(pair->name);
    if (quoteIfSpaces && hasWhiteSpace(pair->name))
        count += 2;
    }
count += elCount;
if (count == 0)
    return NULL;

char *str = needMem(count+5); // A bit of slop

char *strPtr = str;
for (pair = list; pair != NULL; pair = pair->next, strPtr += strlen(strPtr))
    {
    if (pair != list)
        *strPtr++ = delimiter;
    if (hasWhiteSpace(pair->name))
        {
        if (quoteIfSpaces)
            snprintf(strPtr,(count+4)-(strPtr-str),"\"%s\"",pair->name);
        else
            {
            if (delimiter == ' ')  // if delimied by commas, this is entirely okay!
                warn("slPairListToString() Unexpected white space in name delimited by space: "
                     "[%s]\n", pair->name);
            snprintf(strPtr,(count+4)-(strPtr-str),"%s",pair->name); // warn but still make string
            }
        }
    else
        snprintf(strPtr,(count+4)-(strPtr-str),"%s",pair->name);
    }
return str;
}

int slPairCmpCase(const void *va, const void *vb)
/* Compare two slPairs, ignore case. */
{
const struct slPair *a = *((struct slPair **)va);
const struct slPair *b = *((struct slPair **)vb);
return strcasecmp(a->name, b->name);
}

void slPairSortCase(struct slPair **pList)
/* Sort slPair list, ignore case. */
{
slSort(pList, slPairCmpCase);
}

int slPairCmp(const void *va, const void *vb)
/* Compare two slPairs. */
{
const struct slPair *a = *((struct slPair **)va);
const struct slPair *b = *((struct slPair **)vb);
return strcmp(a->name, b->name);
}

int slPairValCmpCase(const void *va, const void *vb)
/* Case insensitive compare two slPairs on their values (must be string). */
{
const struct slPair *a = *((struct slPair **)va);
const struct slPair *b = *((struct slPair **)vb);
return strcasecmp((char *)(a->val), (char *)(b->val));
}

int slPairValCmp(const void *va, const void *vb)
/* Compare two slPairs on their values (must be string). */
{
const struct slPair *a = *((struct slPair **)va);
const struct slPair *b = *((struct slPair **)vb);
return strcmp((char *)(a->val), (char *)(b->val));
}

void slPairValSortCase(struct slPair **pList)
/* Sort slPair list on values (must be string), ignore case. */
{
slSort(pList, slPairValCmpCase);
}

void slPairValSort(struct slPair **pList)
/* Sort slPair list on values (must be string). */
{
slSort(pList, slPairValCmp);
}

int slPairIntCmp(const void *va, const void *vb)
// Compare two slPairs on their integer values.
{
const struct slPair *a = *((struct slPair **)va);
const struct slPair *b = *((struct slPair **)vb);
return ((char *)(a->val) - (char *)(b->val)); // cast works and val is 0 vased integer
}

void slPairIntSort(struct slPair **pList)
// Sort slPair list on integer values.
{
slSort(pList, slPairIntCmp);
}

int slPairAtoiCmp(const void *va, const void *vb)
// Compare two slPairs on their strings interpreted as integer values.
{
const struct slPair *a = *((struct slPair **)va);
const struct slPair *b = *((struct slPair **)vb);
return (atoi((char *)(a->val)) - atoi((char *)(b->val)));
}

void slPairValAtoiSort(struct slPair **pList)
// Sort slPair list on string values interpreted as integers.
{
slSort(pList, slPairAtoiCmp);
}

int differentWord(char *s1, char *s2)
/* strcmp ignoring case - returns zero if strings are
 * the same (ignoring case) otherwise returns difference
 * between first non-matching characters. */
{
char c1, c2;
for (;;)
    {
    c1 = toupper(*s1++);
    c2 = toupper(*s2++);
    if (c1 != c2) /* Takes care of end of string in one but not the other too */
	return c2-c1;
    if (c1 == 0)  /* Take care of end of string in both. */
	return 0;
    }
}

boolean isEmptyTextField(char *s)
/* Recognize NULL or dot as empty text */
{
return (isEmpty(s) || sameString(".", s));
}

boolean startsWith(const char *start, const char *string)
/* Returns TRUE if string begins with start. */
{
char c;
int i;

for (i=0; ;i += 1)
    {
    if ((c = start[i]) == 0)
        return TRUE;
    if (string[i] != c)
        return FALSE;
    }
}

boolean startsWithNoCase(const char *start, const char *string)
/* Returns TRUE if string begins with start, case-insensitive. */
{
char c;
int i;

for (i=0; ;i += 1)
    {
    if ((c = tolower(start[i])) == 0)
        return TRUE;
    if (tolower(string[i]) != c)
        return FALSE;
    }
}

boolean startsWithWord(char *firstWord, char *line)
/* Return TRUE if first white-space-delimited word in line
 * is same as firstWord.  Comparison is case sensitive. */
{
int len = strlen(firstWord);
int i;
for (i=0; i<len; ++i)
   if (firstWord[i] != line[i])
       return FALSE;
char c = line[len];
return c == 0 || isspace(c);
}

char *rStringIn(char *needle, char *haystack)
/* Return last position of needle in haystack, or NULL if it's not there. */
{
int nSize = strlen(needle);
char *pos;
for (pos = haystack + strlen(haystack) - nSize; pos >= haystack; pos -= 1)
    {
    if (memcmp(needle, pos, nSize) == 0)
        return pos;
    }
return NULL;
}

char *nextStringBetween(char *start, char *end, char **pHaystack)
/* Return next string that occurs between start and end strings
 * starting seach at *pHaystack.  This will update *pHaystack to after 
 * end, so it can be called repeatedly. Returns NULL when
 * no more to be found*/
{
char *pos, *p;
int len;
char *haystack = *pHaystack;
if (isEmpty(haystack))
    return NULL;
if ((p = stringIn(start, haystack)) != NULL)
    {
    pos = p + strlen(start);
    if (isEmpty(end))
        return cloneString(pos);
    if ((p = stringIn(end, pos)) != NULL)
        {
        len = p - pos;
        pos = cloneMem(pos, len + 1);
        pos[len] = 0;
	*pHaystack = p;
        return pos;
        }
    }
*pHaystack = NULL;
return NULL;
}

char *stringBetween(char *start, char *end, char *haystack)
/* Return string between start and end strings, or NULL if
 * none found.  The first such instance is returned.
 * String must be freed by caller. */
{
return nextStringBetween(start, end, &haystack);
}

boolean endsWith(char *string, char *end)
/* Returns TRUE if string ends with end. */
{
int sLen, eLen, offset;
sLen = strlen(string);
eLen = strlen(end);
offset = sLen - eLen;
if (offset < 0)
    return FALSE;
return sameString(string+offset, end);
}

char lastChar(char *s)
/* Return last character in string. */
{
if (s == NULL || s[0] == 0)
    return 0;
return s[strlen(s)-1];
}

void trimLastChar(char *s)
/* Erase last character in string. */
{
int len = strlen(s);
if (len > 0)
   s[len-1] = 0;
}

char *lastNonwhitespaceChar(char *s)
// Return pointer to last character in string that is not whitespace.
{
if (s == NULL || s[0] == 0)
    return NULL;

char *sPos = s + (strlen(s) - 1);
for (;sPos >= s;sPos--)
    {
    if (!isspace(*sPos))
        return sPos;
    }
return NULL;
}

char *matchingCharBeforeInLimits(char *limit, char *s, char c)
/* Look for character c sometime before s, but going no further than limit.
 * Return NULL if not found. */
{
while (--s >= limit)
    if (*s == c)
        return s;
return NULL;
}

char *memMatch(char *needle, int nLen, char *haystack, int hLen)
/* Returns first place where needle (of nLen chars) matches
 * haystack (of hLen chars) */
{
char c = *needle++;
nLen -= 1;
hLen -= nLen;
while (--hLen >= 0)
    {
    if (*haystack++ == c && memcmp(needle, haystack, nLen) == 0)
        {
        return haystack-1;
        }
    }
return NULL;
}

void toUpperN(char *s, int n)
/* Convert a section of memory to upper case. */
{
int i;
for (i=0; i<n; ++i)
    s[i] = toupper(s[i]);
}

void toLowerN(char *s, int n)
/* Convert a section of memory to lower case. */
{
int i;
for (i=0; i<n; ++i)
    s[i] = tolower(s[i]);
}

void toggleCase(char *s, int size)
/* toggle upper and lower case chars in string. */
{
char c;
int i;
for (i=0; i<size; ++i)
    {
    c = s[i];
    if (isupper(c))
        c = tolower(c);
    else if (islower(c))
        c = toupper(c);
    s[i] = c;
    }
}


char *strUpper(char *s)
/* Convert entire string to upper case. */
{
char c;
char *ss=s;
for (;;)
    {
    if ((c = *ss) == 0) break;
    *ss++ = toupper(c);
    }
return s;
}

void replaceChar(char *s, char oldc, char newc)
/* Repace one char with another. Modifies original string. */
{
if (!s)
    return;
char c;
while((c=*s))
    {
    if (c == oldc)
       *s = newc;	
    ++s;
    }
}

char *replaceChars(char *string, char *old, char *new)
/*
  Replaces the old with the new. The old and new string need not be of equal size
 Can take any length string.
 Return value needs to be freeMem'd.
*/
{
int numTimes = 0;
int oldLen = strlen(old);
int newLen = strlen(new);
int strLen = 0;
char *result = NULL;
char *ptr = strstr(string, old);
char *resultPtr = NULL;

while(NULL != ptr)
    {
    numTimes++;
    ptr += oldLen;
    ptr = strstr(ptr, old);
    }
strLen = max(strlen(string) + (numTimes * (newLen - oldLen)), strlen(string));
result = needMem(strLen + 1);

ptr = strstr(string, old);
resultPtr = result;
while(NULL != ptr)
    {
    strLen = ptr - string;
    strcpy(resultPtr, string);
    string = ptr + oldLen;

    resultPtr += strLen;
    strcpy(resultPtr, new);
    resultPtr += newLen;
    ptr = strstr(string, old);
    }

strcpy(resultPtr, string);
return result;
}

int strSwapStrs(char *string, int sz,char *oldStr, char *newStr)
/* Swaps all occurrences of the old with the new in string. Need not be same size
   Swaps in place but restricted by sz.  Returns count of swaps or -1 for sz failure. */
{
// WARNING: called at low level, so no errors allowed.
int count = 0;
char *p=NULL;
for(p=strstr(string,oldStr);p!=NULL;p=strstr(p+strlen(oldStr),oldStr))
    count++;
if (count == 0)
    return 0;
if((strlen(string)+(count*(strlen(newStr) - strlen(oldStr))))>=sz)
    return -1;
for(p=strstr(string,oldStr);p!=NULL;p=strstr(p+strlen(newStr),oldStr))
    {
    memmove(p+strlen(newStr),p+strlen(oldStr),strlen(p+strlen(oldStr))+1); // NULL at end is also moved!
    memcpy(p,newStr,strlen(newStr));
    }
return count;
}

char *strLower(char *s)
/* Convert entire string to lower case */
{
char c;
char *ss=s;
for (;;)
    {
    if ((c = *ss) == 0) break;
    *ss++ = tolower(c);
    }
return s;
}

char * memSwapChar(char *s, int len, char oldChar, char newChar)
/* Substitute newChar for oldChar throughout memory of given length. */
{
int ix=0;
for (;ix<len;ix++)
    {
    if (s[ix] == oldChar)
        s[ix] =  newChar;
    }
return s;
}

void stripChar(char *s, char c)
/* Remove all occurences of c from s. */
{
char *in = s, *out = s;
char b;

for (;;)
    {
    b = *out = *in++;
    if (b == 0)
       break;
    if (b != c)
       ++out;
    }
}

char *stripEnclosingChar(char *inout,char encloser)
// Removes enclosing char if found at both beg and end, preserving pointer
// Note: handles brackets '(','{' and '[' by complement at end
{
if (inout == NULL || strlen(inout) < 2 || *inout != encloser)
    return inout;

char *end = inout + (strlen(inout) - 1);
char closer = encloser;
switch (closer)
    {
    case '(': closer = ')'; break;
    case '{': closer = '}'; break;
    case '[': closer = ']'; break;
    default: break;
    }
if (*end  != closer)
    return inout;
*end = '\0';
return memmove(inout,inout+1,strlen(inout));  // use memmove to safely copy in place
}

void stripString(char *s, char *strip)
/* Remove all occurences of strip from s. */
{
char c, *in = s, *out = s;
int stripSize = strlen(strip);
char stripFirst = strip[0];

while ((c = *in) != 0)
    {
    c = *in;
    if (c == stripFirst)
        {
	if (startsWith(strip, in))
	    {
	    in += stripSize;
	    continue;
	    }
	}
    *out = c;
    ++out;
    ++in;
    }
*out = 0;
}

int countCase(char *s,boolean upper)
// Count letters with case (upper or lower)
{
char a;
int count = 0;
while ((a = *s++) != 0)
    if (( upper && isupper(a))
    ||  (!upper && islower(a)))
        ++count;
return count;
}

int countChars(char *s, char c)
/* Return number of characters c in string s. */
{
char a;
int count = 0;
while ((a = *s++) != 0)
    if (a == c)
        ++count;
return count;
}

int countCharsN(char *s, char c, int size)
/* Return number of characters c in string s of given size. */
{
int i;
int count = 0;
for (i=0; i<size; ++i)
    if (s[i] == c)
        ++count;
return count;
}

int countLeadingChars(char *s, char c)
/* Count number of characters c at start of string. */
{
int count = 0;
while (*s++ == c)
   ++count;
return count;
}

int countLeadingDigits(const char *s)
/* Return number of leading digits in s */
{
int count = 0;
while (isdigit(*s))
   {
   ++count;
   ++s;
   }
return count;
}

int countLeadingNondigits(const char *s)
/* Count number of leading non-digit characters in s. */
{
int count = 0;
char c;
while ((c = *s++) != 0)
   {
   if (isdigit(c))
       break;
   ++count;
   }
return count;
}

int countSeparatedItems(char *string, char separator)
/* Count number of items in string you would parse out with given
 * separator,  assuming final separator is optional. */
{
int count = 0;
char c, lastC = 0;
while ((c = *string++) != 0)
    {
    if (c == separator)
       ++count;
    lastC = c;
    }
if (lastC != separator && lastC != 0)
    ++count;
return count;
}

int cmpStringsWithEmbeddedNumbers(const char *a, const char *b)
/* Compare strings such as gene names that may have embedded numbers,
 * so that bmp4a comes before bmp14a */
{
for (;;)
   {
   /* Figure out number of digits at start, and do numerical comparison if there
    * are any.  If numbers agree step over numerical part, otherwise return difference. */
   int aNum = countLeadingDigits(a);
   int bNum = countLeadingDigits(b);
   if (aNum >= 0 && bNum >= 0)
       {
       int diff = atoi(a) - atoi(b);
       if (diff != 0)
           return diff;
       a += aNum;
       b += bNum;
       }

   /* Count number of non-digits at start. */
   int aNonNum = countLeadingNondigits(a);
   int bNonNum = countLeadingNondigits(b);

   // If different sizes of non-numerical part, then don't match, let strcmp sort out how
   if (aNonNum != bNonNum)
        return strcmp(a,b);
   // If no characters left then they are the same!
   else if (aNonNum == 0)
       return 0;
   // Non-numerical part is the same length and non-zero.  See if it is identical.  Return if not.
   else
       {
        int diff = memcmp(a,b,aNonNum);
       if (diff != 0)
            return diff;
       a += aNonNum;
       b += bNonNum;
       }
   }
}

int countSame(char *a, char *b)
/* Count number of characters that from start in a,b that are same. */
{
char c;
int i;
int count = 0;
for (i=0; ; ++i)
   {
   c = a[i];
   if (b[i] != c)
       break;
   if (c == 0)
       break;
   ++count;
   }
return count;
}


/* int chopString(in, sep, outArray, outSize); */
/* This chops up the input string (cannabilizing it)
 * into an array of zero terminated strings in
 * outArray.  It returns the number of strings.
 * If you pass in NULL for outArray, it will just
 * return the number of strings that it *would*
 * chop. This splits the string.
 * GOTCHA: since multiple separators are skipped
 * and treated as one, it is impossible to parse
 * a list with an empty string.
 * e.g. cat\t\tdog returns only cat and dog but no empty string */
int chopString(char *in, const char *sep, char *outArray[], int outSize)
{
int recordCount = 0;

for (;;)
    {
    if (outArray != NULL && recordCount >= outSize)
	break;
    /* Skip initial separators. */
    in += strspn(in, sep);
    if (*in == 0)
	break;
    if (outArray != NULL)
	outArray[recordCount] = in;
    recordCount += 1;
    in += strcspn(in, sep);
    if (*in == 0)
	break;
    if (outArray != NULL)
	*in = 0;
    in += 1;
    }
return recordCount;
}

int chopByWhite(char *in, char *outArray[], int outSize)
/* Like chopString, but specialized for white space separators.
 * See the GOTCHA in chopString */
{
int recordCount = 0;
char c;
for (;;)
    {
    if (outArray != NULL && recordCount >= outSize)
	break;

    /* Skip initial separators. */
    while (isspace(*in)) ++in;
    if (*in == 0)
        break;

    /* Store start of word and look for end of word. */
    if (outArray != NULL)
        outArray[recordCount] = in;
    recordCount += 1;
    for (;;)
        {
        if ((c = *in) == 0)
            break;
        if (isspace(c))
            break;
        ++in;
        }
    if (*in == 0)
	break;

    /* Tag end of word with zero. */
    if (outArray != NULL)
	*in = 0;
    /* And skip over the zero. */
    in += 1;
    }
return recordCount;
}

int chopByChar(char *in, char chopper, char *outArray[], int outSize)
/* Chop based on a single character. */
{
int i;
char c;
if (*in == 0)
    return 0;
for (i=0; (i<outSize) || (outArray==NULL); ++i)
    {
    if (outArray != NULL)
        outArray[i] = in;
    for (;;)
	{
	if ((c = *in++) == 0)
	    return i+1;
	else if (c == chopper)
	    {
            if (outArray != NULL)
                in[-1] = 0;
	    break;
	    }
	}
    }
return i;
}

char crLfChopper[] = "\n\r";
char whiteSpaceChopper[] = " \t\n\r";


char *skipBeyondDelimit(char *s,char delimit)
/* Returns NULL or pointer to first char beyond one (or more contiguous) delimit char.
   If delimit is ' ' then skips beyond first patch of whitespace. */
{
if (s != NULL)
    {
    char *beyond = NULL;
    if (delimit == ' ')
        return skipLeadingSpaces(skipToSpaces(s));
    else
        beyond = strchr(s,delimit);
    if (beyond != NULL)
        {
        for (beyond++;*beyond == delimit;beyond++) ;
        if (*beyond != '\0')
            return beyond;
        }
    }
return NULL;
}

char *skipLeadingSpaces(const char *stringIn)
/* Return first non-white space. */
{
char c, *s = (char *)stringIn;
if (s == NULL) return NULL;
for (;;)
    {
    c = *s;
    if (!isspace(c))
	return s;
    ++s;
    }
}

char *skipToSpaces(const char *stringIn)
/* Return first white space or NULL if none.. */
{
char c, *s = (char *)stringIn;
if (s == NULL)
    return NULL;
for (;;)
    {
    c = *s;
    if (c == 0)
        return NULL;
    if (isspace(c))
	return s;
    ++s;
    }
}



int eraseTrailingSpaces(char *s)
/* Replace trailing white space with zeroes. Returns number of
 * spaces erased. */
{
int len = strlen(s);
int i;
char c;
int erased = 0;

for (i=len-1; i>=0; --i)
    {
    c = s[i];
    if (isspace(c))
	{
	s[i] = 0;
	++erased;
	}
    else
	break;
    }
return erased;
}

/* Remove white space from a string */
void eraseWhiteSpace(char *s)
{
char *in, *out;
char c;

in = out = s;
for (;;)
    {
    c = *in++;
    if (c == 0)
	break;
    if (!isspace(c))
	*out++ = c;
    }
*out++ = 0;
}

/* Remove any chars leaving digits only */
void eraseNonDigits(char *s)
{
char *in, *out;
char c;

in = out = s;
for (;;)
    {
    c = *in++;
    if (c == 0)
        break;
    if (isdigit(c))
        *out++ = c;
    }
*out = 0;
}

/* Remove non-alphanumeric chars from string */
void eraseNonAlphaNum(char *s)
{
char *in, *out;
char c;

in = out = s;
for (;;)
    {
    c = *in++;
    if (c == 0)
        break;
    if (isalnum(c))
        *out++ = c;
    }
*out = 0;
}

char *trimSpaces(char *s)
/* Remove leading and trailing white space. */
{
if (s != NULL)
    {
    s = skipLeadingSpaces(s);
    eraseTrailingSpaces(s);
    }
return s;
}

void repeatCharOut(FILE *f, char c, int count)
/* Write character to file repeatedly. */
{
while (--count >= 0)
    fputc(c, f);
}

void spaceOut(FILE *f, int count)
/* Put out some spaces to file. */
{
repeatCharOut(f, ' ', count);
}

void starOut(FILE *f, int count)
/* Put out some asterisks to file. */
{
repeatCharOut(f, '*', count);
}

boolean hasWhiteSpace(char *s)
/* Return TRUE if there is white space in string. */
{
char c;
while ((c = *s++) != 0)
    if (isspace(c))
        return TRUE;
return FALSE;
}

char *nextWord(char **pLine)
/* Return next word in *pLine and advance *pLine to next
 * word. */
{
char *s = *pLine, *e;
if (s == NULL || s[0] == 0)
    return NULL;
s = skipLeadingSpaces(s);
if (s[0] == 0)
    return NULL;
e = skipToSpaces(s);
if (e != NULL)
    *e++ = 0;
*pLine = e;
return s;
}

int ptArrayIx(void *pt, void *array, int arraySize)
/* Return index of pt in array or -1 if not there. */
{
int i;
void **a = array;
for (i=0; i<arraySize; ++i)
    {
    if (pt == a[i])
        return i;
    }
return -1;
}

FILE *mustOpen(const char *fileName, char *mode)
/* Open a file - or squawk and die. */
{
FILE *f;

if (sameString(fileName, "stdin"))
    return stdin;
if ((f = fopen(fileName, mode)) == NULL)
    {
    char *modeName = "";
    if (mode)
        {
        if (mode[0] == 'r')
            modeName = " to read";
        else if (mode[0] == 'w')
            modeName = " to write";
        else if (mode[0] == 'a')
            modeName = " to append";
        }
    errAbort("mustOpen: Can't open %s%s: %s", fileName, modeName, strerror(errno));
    }
return f;
}

void mustWrite(FILE *file, void *buf, size_t size)
/* Write to a file or squawk and die. */
{
if (size != 0 && fwrite(buf, size, 1, file) != 1)
    {
    errAbort("Error writing %lld bytes: %s\n", (long long)size, strerror(ferror(file)));
    }
}


void mustRead(FILE *file, void *buf, size_t size)
/* Read size bytes from a file or squawk and die. */
{
if (size != 0 && fread(buf, size, 1, file) != 1)
    {
    if (ferror(file))
	errAbort("Error reading %lld bytes: %s", (long long)size, strerror(ferror(file)));
    else
	errAbort("End of file reading %lld bytes", (long long)size);
    }
}

void writeString(FILE *f, char *s)
/* Write a 255 or less character string to a file.  Truncate if longer.  This
 * will write the length of the string in the first byte then the string
 * itself. */
{
UBYTE bLen;
int len = strlen(s);

if (len > 255)
    {
    warn("String too long in writeString (%d chars):\n%s", len, s);
    len = 255;
    }
bLen = len;
writeOne(f, bLen);
mustWrite(f, s, len);
}

void writeStringSafe(FILE *f, char *s)
/* Write a 255 or less character string to a file.  Generate an error if
 * longer.  This will write the length of the string in the first byte then
 * the string itself. */
{
if (strlen(s) > 255)
    errAbort("attempt to write string longer than 255 bytes");
writeString(f, s);
}


char *readString(FILE *f)
/* Read a string (written with writeString) into
 * memory.  freeMem the result when done. */
{
UBYTE bLen;
int len;
char *s;

if (!readOne(f, bLen))
    return NULL;
len = bLen;
s = needMem(len+1);
if (len > 0)
    mustRead(f, s, len);
return s;
}

boolean fastReadString(FILE *f, char buf[256])
/* Read a string into buffer, which must be long enough
 * to hold it.  String is in 'writeString' format. */
{
UBYTE bLen;
int len;
if (!readOne(f, bLen))
    return FALSE;
if ((len = bLen)> 0)
    mustRead(f, buf, len);
buf[len] = 0;
return TRUE;
}

void mustGetLine(FILE *file, char *buf, int charCount)
/* Read at most charCount-1 bytes from file, but stop after newline if one is
 * encountered.  The string in buf is '\0'-terminated.  (See man 3 fgets.)
 * Die if there is an error. */
{
char *success = fgets(buf, charCount, file);
if (success == NULL && charCount > 0)
    buf[0] = '\0';
if (ferror(file))
    errAbort("mustGetLine: fgets failed: %s", strerror(ferror(file)));
}


static char *getWhenceStr(int whence)
/* get string description of fseek/lseek whence parameter */
{
return ((whence == SEEK_SET) ? "SEEK_SET" : (whence == SEEK_CUR) ? "SEEK_CUR" :
        (whence == SEEK_END) ? "SEEK_END" : "invalid 'whence' value");

}

void mustSeek(FILE *file, off_t offset, int whence)
/* Seek to given offset, relative to whence (see man fseek) in file or errAbort. */
{
int ret = fseek(file, offset, whence);
if (ret < 0)
    errnoAbort("fseek(%lld, %s (%d)) failed", (long long)offset, getWhenceStr(whence), whence);
}

int mustOpenFd(char *fileName, int flags)
/* Open a file descriptor (see man 2 open) or squawk and die. */
{
if (sameString(fileName, "stdin"))
    return STDIN_FILENO;
// mode is necessary when O_CREAT is given, ignored otherwise
int mode = 0666;
int fd = open(fileName, flags, mode);
if (fd < 0)
    {
    char *modeName = "";
    if ((flags & (O_WRONLY | O_CREAT | O_TRUNC)) == (O_WRONLY | O_CREAT | O_TRUNC))
	modeName = " to create and truncate";
    else if ((flags & (O_WRONLY | O_CREAT)) == (O_WRONLY | O_CREAT))
	modeName = " to create";
    else if ((flags & O_WRONLY) == O_WRONLY)
	modeName = " to write";
    else if ((flags & O_RDWR) == O_RDWR)
	modeName = " to append";
    else
	modeName = " to read";
    errnoAbort("mustOpenFd: Can't open %s%s", fileName, modeName);
    }
return fd;
}

void mustReadFd(int fd, void *buf, size_t size)
/* Read size bytes from a file or squawk and die. */
{
ssize_t actualSize;
char *cbuf = buf;
// using a loop because linux was not returning all data in a single request when request size exceeded 2GB.
// MacOS complains invalid argument if it is over 2GB
while (size > 0)
    {
    actualSize = read(fd, cbuf, min(0x7FFF000,size));  // max 2GB 0x7FFF000 MAX_RW_COUNT = (INT_MAX & PAGE_MASK)
    if (actualSize < 0)
	errnoAbort("Error reading %lld bytes", (long long)size);
    if (actualSize == 0)
	errAbort("End of file reading %llu bytes (got %lld)", (unsigned long long)size, (long long)actualSize);
    cbuf += actualSize;
    size -= actualSize;
    }
}

void mustWriteFd(int fd, void *buf, size_t size)
/* Write size bytes to file descriptor fd or die.  (See man 2 write.) */
{
ssize_t result = write(fd, buf, size);
if (result < size)
    {
    if (result < 0)
	errnoAbort("mustWriteFd: write failed");
    else
        errAbort("mustWriteFd only wrote %lld of %lld bytes. Likely the disk is full.",
	    (long long)result, (long long)size);
    }
}

void mustCloseFd(int *pFd)
/* Close file descriptor *pFd if >= 0, abort if there's an error, set *pFd = -1. */
{
if (pFd != NULL && *pFd >= 0)
    {
    if (close(*pFd) < 0)
	errnoAbort("close failed");
    *pFd = -1;
    }
}

boolean carefulCloseWarn(FILE **pFile)
/* Close file if open and null out handle to it.
 * Return FALSE and print a warning message if there
 * is a problem.*/
{
FILE *f;
boolean ok = TRUE;
if ((pFile != NULL) && ((f = *pFile) != NULL))
    {
    if (f != stdin)
        {
        if (fclose(f) != 0)
	    {
            warn("%s\n%s", strerror(errno), "fclose failed");
	    ok = FALSE;
	    }
        }
    *pFile = NULL;
    }
return ok;
}

void carefulClose(FILE **pFile)
/* Close file if open and null out handle to it.
 * Warn and abort if there's a problem. */
{
if (!carefulCloseWarn(pFile))
    noWarnAbort();
}

bits64 byteSwap64(bits64 a)
/* Return byte-swapped version of a */
{
union {bits64 whole; UBYTE bytes[8];} u,v;
u.whole = a;
v.bytes[0] = u.bytes[7];
v.bytes[1] = u.bytes[6];
v.bytes[2] = u.bytes[5];
v.bytes[3] = u.bytes[4];
v.bytes[4] = u.bytes[3];
v.bytes[5] = u.bytes[2];
v.bytes[6] = u.bytes[1];
v.bytes[7] = u.bytes[0];
return v.whole;
}

bits64 readBits64(FILE *f, boolean isSwapped)
/* Read and optionally byte-swap 64 bit entity. */
{
bits64 val;
mustReadOne(f, val);
if (isSwapped)
    val = byteSwap64(val);
return val;
}

bits64 fdReadBits64(int fd, boolean isSwapped)
/* Read and optionally byte-swap 64 bit entity. */
{
bits64 val;
mustReadOneFd(fd, val);
if (isSwapped)
    val = byteSwap64(val);
return val;
}

bits64 memReadBits64(char **pPt, boolean isSwapped)
/* Read and optionally byte-swap 64 bit entity from memory buffer pointed to by
 * *pPt, and advance *pPt past read area. */
{
bits64 val;
memcpy(&val, *pPt, sizeof(val));
if (isSwapped)
    val = byteSwap64(val);
*pPt += sizeof(val);
return val;
}

bits32 byteSwap32(bits32 a)
/* Return byte-swapped version of a */
{
union {bits32 whole; UBYTE bytes[4];} u,v;
u.whole = a;
v.bytes[0] = u.bytes[3];
v.bytes[1] = u.bytes[2];
v.bytes[2] = u.bytes[1];
v.bytes[3] = u.bytes[0];
return v.whole;
}

bits32 readBits32(FILE *f, boolean isSwapped)
/* Read and optionally byte-swap 32 bit entity. */
{
bits32 val;
mustReadOne(f, val);
if (isSwapped)
    val = byteSwap32(val);
return val;
}

bits32 fdReadBits32(int fd, boolean isSwapped)
/* Read and optionally byte-swap 32 bit entity. */
{
bits32 val;
mustReadOneFd(fd, val);
if (isSwapped)
    val = byteSwap32(val);
return val;
}

bits32 memReadBits32(char **pPt, boolean isSwapped)
/* Read and optionally byte-swap 32 bit entity from memory buffer pointed to by
 * *pPt, and advance *pPt past read area. */
{
bits32 val;
memcpy(&val, *pPt, sizeof(val));
if (isSwapped)
    val = byteSwap32(val);
*pPt += sizeof(val);
return val;
}

bits16 byteSwap16(bits16 a)
/* Return byte-swapped version of a */
{
union {bits16 whole; UBYTE bytes[2];} u,v;
u.whole = a;
v.bytes[0] = u.bytes[1];
v.bytes[1] = u.bytes[0];
return v.whole;
}

bits16 readBits16(FILE *f, boolean isSwapped)
/* Read and optionally byte-swap 16 bit entity. */
{
bits16 val;
mustReadOne(f, val);
if (isSwapped)
    val = byteSwap16(val);
return val;
}

bits16 fdReadBits16(int fd, boolean isSwapped)
/* Read and optionally byte-swap 16 bit entity. */
{
bits16 val;
mustReadOneFd(fd, val);
if (isSwapped)
    val = byteSwap16(val);
return val;
}

bits16 memReadBits16(char **pPt, boolean isSwapped)
/* Read and optionally byte-swap 16 bit entity from memory buffer pointed to by
 * *pPt, and advance *pPt past read area. */
{
bits16 val;
memcpy(&val, *pPt, sizeof(val));
if (isSwapped)
    val = byteSwap16(val);
*pPt += sizeof(val);
return val;
}

double byteSwapDouble(double a)
/* Return byte-swapped version of a */
{
union {double whole; UBYTE bytes[8];} u,v;
u.whole = a;
v.bytes[0] = u.bytes[7];
v.bytes[1] = u.bytes[6];
v.bytes[2] = u.bytes[5];
v.bytes[3] = u.bytes[4];
v.bytes[4] = u.bytes[3];
v.bytes[5] = u.bytes[2];
v.bytes[6] = u.bytes[1];
v.bytes[7] = u.bytes[0];
return v.whole;
}


double readDouble(FILE *f, boolean isSwapped)
/* Read and optionally byte-swap double-precision floating point entity. */
{
double val;
mustReadOne(f, val);
if (isSwapped)
    val = byteSwapDouble(val);
return val;
}

double memReadDouble(char **pPt, boolean isSwapped)
/* Read and optionally byte-swap double-precision floating point entity
 * from memory buffer pointed to by *pPt, and advance *pPt past read area. */
{
double val;
memcpy(&val, *pPt, sizeof(val));
if (isSwapped)
    val = byteSwapDouble(val);
*pPt += sizeof(val);
return val;
}

float byteSwapFloat(float a)
/* Return byte-swapped version of a */
{
union {float whole; UBYTE bytes[4];} u,v;
u.whole = a;
v.bytes[0] = u.bytes[3];
v.bytes[1] = u.bytes[2];
v.bytes[2] = u.bytes[1];
v.bytes[3] = u.bytes[0];
return v.whole;
}


float readFloat(FILE *f, boolean isSwapped)
/* Read and optionally byte-swap single-precision floating point entity. */
{
float val;
mustReadOne(f, val);
if (isSwapped)
    val = byteSwapFloat(val);
return val;
}

float memReadFloat(char **pPt, boolean isSwapped)
/* Read and optionally byte-swap single-precision floating point entity
 * from memory buffer pointed to by *pPt, and advance *pPt past read area. */
{
float val;
memcpy(&val, *pPt, sizeof(val));
if (isSwapped)
    val = byteSwapFloat(val);
*pPt += sizeof(val);
return val;
}

char* readLine(FILE* fh)
/* Read a line of any size into dynamic memory, return null on EOF */
{
int bufCapacity = 256;
int bufSize = 0;
char* buf = needMem(bufCapacity);
int ch;

/* loop until EOF of EOLN */
while (((ch = getc(fh)) != EOF) && (ch != '\n'))
    {
    /* expand if almost full, always keep one extra char for
     * zero termination */
    if (bufSize >= bufCapacity-2)
        {
        bufCapacity *= 2;
        buf = realloc(buf, bufCapacity);
        if (buf == NULL)
            {
            errAbort("Out of memory in readline - request size %d bytes", bufCapacity);
            }
        }
    buf[bufSize++] = ch;
    }

/* only return EOF if no data was read */
if ((ch == EOF) && (bufSize == 0))
    {
    freeMem(buf);
    return NULL;
    }
buf[bufSize] = '\0';
return buf;
}

int vasafef(char* buffer, int bufSize, char *format, va_list args)
/* Format string to buffer, vsprintf style, only with buffer overflow
 * checking.  The resulting string is always terminated with zero byte. */
{
int sz = vsnprintf(buffer, bufSize, format, args);
/* note that some version return -1 if too small */
if ((sz < 0) || (sz >= bufSize))
    {
    buffer[bufSize-1] = (char) 0;
    errAbort("buffer overflow, size %d, format: %s, buffer: '%s'", bufSize, format, buffer);
    }
return sz;
}

int safef(char* buffer, int bufSize, char *format, ...)
/* Format string to buffer, vsprintf style, only with buffer overflow
 * checking.  The resulting string is always terminated with zero byte. */
{
int sz;
va_list args;
va_start(args, format);
sz = vasafef(buffer, bufSize, format, args);
va_end(args);
return sz;
}

int safefcat(char* buffer, int bufSize, char *format, ...)
/* Safely format string to the end of the buffer.  Returns number of characters
 * appended. */
{
int sz, len = strlen(buffer);;
va_list args;
va_start(args, format);
sz = vasafef(buffer + len, bufSize - len, format, args);
va_end(args);
return sz;
}


void safecpy(char *buf, size_t bufSize, const char *src)
/* copy a string to a buffer, with bounds checking.*/
{
size_t slen = strlen(src);
if (slen > bufSize-1)
    errAbort("buffer overflow, size %lld, string size: %lld", (long long)bufSize, (long long)slen);
strcpy(buf, src);
}

void safencpy(char *buf, size_t bufSize, const char *src, size_t n)
/* copy n characters from a string to a buffer, with bounds checking.
 * Unlike strncpy, always null terminates the result */
{
if (n > bufSize-1)
    errAbort("buffer overflow, size %lld, substring size: %lld", (long long)bufSize, (long long)n);
// strlen(src) can take a long time when src is for example a pointer into a chromosome sequence.
// Instead of setting slen to max(strlen(src), n), just stop counting length at n.
size_t slen = 0;
while (src[slen] != '\0' && slen < n)
    slen++;
strncpy(buf, src, n);
buf[slen] = '\0';
}

void safecat(char *buf, size_t bufSize, const char *src)
/* Append  a string to a buffer, with bounds checking.*/
{
size_t blen = strlen(buf);
size_t slen = strlen(src);
if (blen+slen > bufSize-1)
    errAbort("buffer overflow, size %lld, new string size: %lld", (long long)bufSize, (long long)(blen+slen));
strcat(buf, src);
}

void safencat(char *buf, size_t bufSize, const char *src, size_t n)
/* append n characters from a string to a buffer, with bounds checking. */
{
size_t blen = strlen(buf);
if (blen+n > bufSize-1)
    errAbort("buffer overflow, size %lld, new string size: %lld", (long long)bufSize, (long long)(blen+n));
size_t slen = strlen(src);
if (slen > n)
    slen = n;
strncat(buf, src, n);
buf[blen+slen] = '\0';
}

void safememset(char *buf, size_t bufSize, const char c, size_t n)
/* Append a character to a buffer repeatedly, n times with bounds checking.*/
{
size_t blen = strlen(buf);
if (blen+n+1 > bufSize)
    errAbort("buffer overflow, size %lld, new string size: %lld", (long long)bufSize, (long long)(blen+n));
memset(buf+blen, c, n);
buf[blen+n] = 0;
}


static char *naStr = "n/a";
static char *emptyStr = "";

char *naForNull(char *s)
/* Return 'n/a' if s is NULL, otherwise s. */
{
if (s == NULL)
   s = naStr;
return s;
}

char *naForEmpty(char *s)
/* Return n/a if s is "" or NULL, otherwise s. */
{
if (s == NULL || s[0] == 0)
    s = naStr;
return s;
}

char *emptyForNull(char *s)
/* Return "" if s is NULL, otherwise s. */
{
if (s == NULL)
   s = emptyStr;
return s;
}

char *nullIfAllSpace(char *s)
/* Return NULL if s is all spaces, otherwise s. */
{
s = skipLeadingSpaces(s);
if (s != NULL)
    if (s[0] == 0)
        s = NULL;
return s;
}

char *trueFalseString(boolean b)
/* Return "true" or "false" */
{
return (b ? "true" : "false");
}

char *skipNumeric(char *s)
/* Return first char of s that's not a digit */
{
while (isdigit(*s))
   ++s;
return s;
}

char *skipToNumeric(char *s)
/* skip up to where numeric digits appear */
{
while (*s != 0 && !isdigit(*s))
    ++s;
return s;
}

char *splitOffNonNumeric(char *s)
/* Split off non-numeric part, e.g. mm of mm8. Result should be freed when done */
{
return cloneStringZ(s,skipToNumeric(s)-s);
}

char *splitOffNumber(char *db)
/* Split off number part, e.g. 8 of mm8. Result should be freed when done */
{
return cloneString(skipToNumeric(db));
}

