
/*
    Project:
    File Name:      StrFun.c
    Author:			ygy
    Date:
    C Compiler:    
    Purpose:        
    Copyright (C) 2009 Inventec Electronics (Nanjing) Co., Ltd.
    All rights reserved.
*/

#include "strfun.h"


/*
	Convert string to long
* Purpose:
	Converts ASCII string pointed to by nptr to binary.
	Overflow is not detected.
* Entry:
	nptr = ptr to string to convert
* Exit:
	return long int value of the string
* Exceptions:
	None - overflow is not detected.
*/

long atol(const char *nptr)
{
	int		c;			/* current char */
	long	total;		/* current total */
	int		sign;		/* if '-', then negative, otherwise positive */

	/* skip whitespace */
	while ( isspace((int)(unsigned char)*nptr) )
		++nptr;

	c = (int)(unsigned char)*nptr++;
	sign = c;           /* save sign indication */
	if (c == '-' || c == '+')
		c = (int)(unsigned char)*nptr++;    /* skip sign */

	total = 0;

	while (isdigit(c)) 
	{
		total = 10 * total + (c - '0');     /* accumulate digit */
		c = (int)(unsigned char)*nptr++;    /* get next char */
	}

	if (sign == '-')
		return -total;
	else
		return total;   /* return result, negated if necessary */
}


/*
	Convert string to int
* Purpose:
	Converts ASCII string pointed to by nptr to binary.
	Overflow is not detected.  Because of this, we can just use
	atol().
* Entry:
	nptr = ptr to string to convert
* Exit:
	return int value of the string
* Exceptions:
	None - overflow is not detected.
*/
int atoi(const char *nptr)
{
    return (int)atol(nptr);
}


/*
	convert binary int to ASCII string
* Purpose:
	Converts an int to a character string.
* Entry:
	val - number to be converted (int, long or unsigned long)
	int radix - base to convert into
	char *buf - ptr to buffer to place result
* Exit:
	fills in space pointed to by buf with string result
	returns a pointer to this buffer
* Exceptions:
*/
static void xtoa ( unsigned long val, char *buf, unsigned radix, int is_neg )
{
	char *p;			/* pointer to traverse string */
	char *firstdig;		/* pointer to first digit */
	char temp;			/* temp char */
	unsigned digval;	/* value of digit */

	p = buf;

	if (is_neg) 
	{
		/* negative, so output '-' and negate */
		*p++ = '-';
		val = (unsigned long)(-(long)val);
	}

	firstdig = p;           /* save pointer to first digit */

	do 
	{
		digval = (unsigned) (val % radix);
		val /= radix;       /* get next digit */

		/* convert to ascii and store */
		if (digval > 9)
			*p++ = (char) (digval - 10 + 'a');  /* a letter */
		else
			*p++ = (char) (digval + '0');       /* a digit */
	} while (val > 0);

	/* We now have the digit of the number in the buffer, but in reverse
	   order.  Thus we reverse them now. */

	*p-- = '\0';            /* terminate string; p points to last digit */

	do 
	{
		temp = *p;
		*p = *firstdig;
		*firstdig = temp;   /* swap *p and *firstdig */
		--p;
		++firstdig;         /* advance to next two digits */
	} while (firstdig < p); /* repeat until halfway */
}

char *itoa ( int val, char *buf, int radix )
{
	if (radix == 10 && val < 0)
		xtoa((unsigned long)val, buf, radix, 1);
	else
		xtoa((unsigned long)(unsigned int)val, buf, radix, 0);

	return buf;
}

char *ltoa ( long val, char *buf, int radix )
{
	xtoa((unsigned long)val, buf, radix, (radix == 10 && val < 0));

	return buf;
}

////////////////////////////////

/*
*	map lower-case characters in a string to upper-case

* Purpose:
	strupr() converts lower-case characters in a null-terminated string
	to their upper-case equivalents.  Conversion is done in place and
	characters other than lower-case letters are not modified.
	
	In the C locale, this function modifies only 7-bit ASCII characters
	in the range 0x61 through 0x7A ('a' through 'z').
	
	If the locale is not the 'C' locale, MapStringW() is used to do
	the work.  Assumes enough space in the string to hold result.
	
* Entry:
	char *string - string to change to upper case
	
* Exit:
	input string address
	
* Exceptions:
	The original string is returned unchanged on any error.
*/
char *strupr ( char * string )
{
    char * cp;

    for (cp=string; *cp; ++cp)
    {
        if ('a' <= *cp && *cp <= 'z')
            *cp += 'A' - 'a';
    }

    return(string);
}


/*
*	search for string2 in string1

* Purpose:
	finds the first occurrence of string2 in string1

* Entry:
	char *string1 - string to search in
	char *string2 - string to search for

* Exit:
	returns a pointer to the first occurrence of string2 in
	string1, or NULL if string2 does not occur in string1

* Uses:

* Exceptions:

*/
char *strstr ( const char * str1, const char * str2 )
{
	char *cp = (char *) str1;
	char *s1, *s2;

	if ( !*str2 )
		return((char *)str1);

	while (*cp)
	{
		s1 = cp;
		s2 = (char *) str2;

		while ( *s1 && *s2 && !(*s1-*s2) )
			s1++, s2++;

		if (!*s2)
			return(cp);

		cp++;
	}

	return NULL;
}


/*
	sets all of string to val

* Purpose:
	Sets all of characters in string (except the terminating '/0'
	character) equal to val.


* Entry:
	char *string - string to modify
	char val - value to fill string with

* Exit:
	returns string -- now filled with val's

* Uses:

* Exceptions:

*/
char *strset ( char * string, int val )
{
	char *start = string;

	while (*string)
		*string++ = (char)val;

	return(start);
}


/*
*	reverse a string in place

* Purpose:
	Reverses the order of characters in the string.  The terminating
	null character remains in place.

* Entry:
	char *string - string to reverse

* Exit:
	returns string - now with reversed characters

* Exceptions:

*/
char * strrev ( char * string )
{
	char *start = string;
	char *left = string;
	char ch;

	while (*string++)  ;               /* find end of string */
	string -= 2;

	while (left < string)
	{
		ch = *left;
		*left++ = *string;
		*string-- = ch;
	}

	return(start);
}


/*
*	find last occurrence of ch in string

* Purpose:
	Finds the last occurrence of ch in string.  The terminating
	null character is used as part of the search.

* Entry:
	char *string - string to search in
	char ch - character to search for
*
* Exit:
	returns a pointer to the last occurrence of ch in the given	string
	returns NULL if ch does not occurr in the string
*
* Exceptions:
*
*/
char *strrchr ( const char * string, int ch )
{
	char *start = (char *)string;

	/* find end of string */
	while (*string++)  ;                  
		
	/* search towards front */
	while (--string != start && *string != (char)ch) ;

	if (*string == (char)ch)                /* char found ? */
		return( (char *)string );

	return(NULL);
}


/*
*	set at most count characters to val

* Purpose:
	Sets the first count characters of string the character value.
	If the length of string is less than count, the length of
	string is used in place of n.

* Entry:
	char *string - string to set characters in
	char val - character to fill with
	unsigned count - count of characters to fill

* Exit:
	returns string, now filled with count copies of val.

* Exceptions:

*/
char *strnset ( char * string, int val, size_t count )
{
    char *start = string;

    while (count-- && *string)
        *string++ = (char)val;

    return(start);
}


/*
*	compares count char of strings, ignore case

* Purpose:
	Compare the two strings for lexical order.  Stops the comparison
	when the following occurs: (1) strings differ, (2) the end of the
	strings is reached, or (3) count characters have been compared.
	For the purposes of the comparison, upper case characters are
	converted to lower case.

* Entry:
	char *first, *last - strings to compare
	size_t count - maximum number of characters to compare

* Exit:
	returns <0 if first < last
	returns 0 if first == last
	returns >0 if first > last

* Exceptions:

*/
int strnicmp ( const char * first, const char * last, size_t count )
{
    int f,l;

    if ( count )
    {
        do 
		{
            if ( ((f = (unsigned char)(*(first++))) >= 'A') &&
                 (f <= 'Z') )
                f -= 'A' - 'a';

            if ( ((l = (unsigned char)(*(last++))) >= 'A') &&
                 (l <= 'Z') )
                l -= 'A' - 'a';

        } while ( --count && f && (f == l) );

        return( f - l );
    }

    return( 0 );
}


/*
*	copy at most n characters

* Purpose:
	Copies count characters from the source string to the
	destination.  If count is less than the length of source,
	NO NULL CHARACTER is put onto the end of the copied string.
	If count is greater than the length of sources, dest is padded
	with null characters to length count.

* Entry:
	char *dest - pointer to destination
	char *source - source string for copy
	unsigned count - max number of characters to copy

* Exit:
	returns dest

* Exceptions:

*/
char *strncpy ( char * dest, const char * source, size_t count )
{
    char *start = dest;

    while (count && (*dest++ = *source++))    /* copy string */
        count--;

    if (count)                              /* pad out with zeroes */
        while (--count)
                *dest++ = '\0';

    return(start);
}



/*
*	compare first count chars of strings

* Purpose:
	Compares two strings for lexical order.  The comparison stops
	after: (1) a difference between the strings is found, (2) the end
	of the strings is reached, or (3) count characters have been
	compared.

* Entry:
	char *first, *last - strings to compare
	unsigned count - maximum number of characters to compare

* Exit:
	returns <0 if first < last
	returns  0 if first == last
	returns >0 if first > last

* Exceptions:

*/
int strncmp ( const char * first, const char * last, size_t count )
{
    if (!count)
        return(0);

    while (--count && *first && *first == *last)
    {
        first++;
        last++;
    }

    return( *(unsigned char *)first - *(unsigned char *)last );
}



/*
*	append count chars of back onto front

* Purpose:
	Appends at most count characters of the string back onto the
	end of front, and ALWAYS terminates with a null character.
	If count is greater than the length of back, the length of back
	is used instead.  (Unlike strncpy, this routine does not pad out
	to count characters).

* Entry:
	char *front - string to append onto
	char *back - string to append
	unsigned count - count of max characters to append

* Exit:
	returns a pointer to string appended onto (front).

* Uses:

* Exceptions:

*/
char *strncat ( char * front, const char * back, size_t count )
{
    char *start = front;

    while (*front++)
            ;
    front--;

    while (count--)
        if (!(*front++ = *back++))
            return(start);

    *front = '\0';

    return(start);
}



/*
*	map upper-case characters in a string to lower-case

* Purpose:
	strlwr() converts upper-case characters in a null-terminated string
	to their lower-case equivalents.  Conversion is done in place and
	characters other than upper-case letters are not modified.

	In the C locale, this function modifies only 7-bit ASCII characters
	in the range 0x41 through 0x5A ('A' through 'Z').

	If the locale is not the 'C' locale, MapString() is used to do
	the work.  Assumes enough space in the string to hold result.

* Entry:
	char *string - string to change to lower case

* Exit:
	input string address

* Exceptions:
	The original string is returned unchanged on any error.

*/
char *strlwr ( char * string )
{
    char * cp;

    for (cp=string; *cp; ++cp)
    {
        if ('A' <= *cp && *cp <= 'Z')
            *cp += 'a' - 'A';
    }

    return(string);
}


/*
*	return the length of a null-terminated string

* Purpose:
	Finds the length in bytes of the given string, not including
	the final null character.

* Entry:
	const char * str - string whose length is to be computed

* Exit:
	length of the string "str", exclusive of the final null byte

* Exceptions:

*/
size_t strlen ( const char * str )
{
    const char *eos = str;

    while( *eos++ ) ;

    return( (int)(eos - str - 1) );
}


/*
*	compare strings, ignore case

* Purpose:
	stricmp/strcmpi perform a case-insensitive string comparision.
	For differences, upper case letters are mapped to lower case.
	Thus, "abc_" < "ABCD" since "_" < "d".

* Entry:
	char *dst, *src - strings to compare

* Return:
	<0 if dst < src
	0 if dst = src
	>0 if dst > src

* Exceptions:

*/
int strcmpi(const char * dst, const char * src)
{
    int f,l;

    do 
	{
        if ( ((f = (unsigned char)(*(dst++))) >= 'A') && (f <= 'Z') )
            f -= ('A' - 'a');

        if ( ((l = (unsigned char)(*(src++))) >= 'A') && (l <= 'Z') )
            l -= ('A' - 'a');
    } while ( f && (f == l) );

    return(f - l);
}

int stricmp ( const char * dst, const char * src )
{
    return( strcmpi(dst,src) );
}



/*
*	compare two strings, returning less than, equal to, or greater than

* Purpose:
	STRCMP compares two strings and returns an integer
	to indicate whether the first is less than the second, the two are
	equal, or whether the first is greater than the second.

	Comparison is done byte by byte on an UNSIGNED basis, which is to
	say that Null (0) is less than any other character (1-255).

* Entry:
	const char * src - string for left-hand side of comparison
	const char * dst - string for right-hand side of comparison

* Exit:
	returns -1 if src <  dst
	returns  0 if src == dst
	returns +1 if src >  dst

* Exceptions:

*/
int strcmp ( const char * src, const char * dst )
{
    int ret = 0 ;

    while( ! (ret = *(unsigned char *)src - *(unsigned char *)dst) && *dst)
        ++src, ++dst;

    if ( ret < 0 )
        ret = -1 ;
    else if ( ret > 0 )
        ret = 1 ;

    return( ret );
}



/*
*	search a string for a character

* Purpose:
	Searches a string for a given character, which may be the
	null character '\0'.

* Entry:
	char *string - string to search in
	char c - character to search for

* Exit:
	returns pointer to the first occurence of c in string
	returns NULL if c does not occur in string

* Exceptions:

*/
char *strchr ( const char * string, int ch )
{
    while (*string && *string != (char)ch)
        string++;

    if (*string == (char)ch)
        return((char *)string);

    return(NULL);
}



/*
*	concatenate (append) one string to another

* Purpose:
	Concatenates src onto the end of dest.  Assumes enough
	space in dest.

* Entry:
	char *dst - string to which "src" is to be appended
	const char *src - string to be appended to the end of "dst"

* Exit:
	The address of "dst"

* Exceptions:

*/
char *strcat ( char * dst, const char * src )
{
    char * cp = dst;

    while( *cp )
        cp++;                   /* find end of dst */

    while( *cp++ = *src++ ) ;   /* Copy src to end of dst */

    return( dst );              /* return dst */
}



/*
*	copy one string over another

* Purpose:
	Copies the string src into the spot specified by
	dest; assumes enough room.

* Entry:
	char * dst - string over which "src" is to be copied
	const char * src - string to be copied over "dst"

* Exit:
	The address of "dst"

* Exceptions:
*/
char *strcpy(char * dst, const char * src)
{
    char * cp = dst;

    while( *cp++ = *src++ )  ;          /* Copy src over dst */

    return( dst );
}



/*
*	duplicate string into malloc'd memory

* Purpose:
	Allocates enough storage via malloc() for a copy of the
	string, copies the string into the new memory, and returns
	a pointer to it.

* Entry:
	char *string - string to copy into new memory

* Exit:
	returns a pointer to the newly allocated storage with the
	string in it.

	returns NULL if enough memory could not be allocated, or
	string was NULL.

* Uses:

* Exceptions:

*/
#include <malloc.h>
char *strdup ( const char * string )
{
	char *memory;

	if (!string)
		return(NULL);

	if (memory = malloc(strlen(string) + 1))
		return(strcpy(memory,string));

	return(NULL);
}

