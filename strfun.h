
/*
    Project:
    File Name:      StrFun.h
    Author:			ygy
    Date:
    C Compiler:    
    Purpose:        
    Copyright (C) 2009 Inventec Electronics (Nanjing) Co., Ltd.
    All rights reserved.
*/

#ifndef _STRFUN_H_
#define _STRFUN_H_



#ifndef isdigit
#define isdigit(c)		((c >= '0') && (c <= '9'))
#endif

#ifndef isspace
#define isspace(c)		(c=='\n' || c=='\t' || c=='\v' || c==' ' || c=='\r' || c=='\f')
#endif

#ifndef isalpha
#define isalpha(a)		(a >= 'A' && a <= 'z')
#endif

#ifndef isascii
#define isascii(a)		( (unsigned)(c) < 0x80 )
#endif

#ifndef toascii
#define toascii(a)		( (c) & 0x7f )
#endif

#ifndef tolower
#define toupper(x)		( (c)-'A'+'a' )
#endif

#ifndef toupper
#define toupper(x)		( (c)-'a'+'A' )
#endif


#ifndef NULL
#define NULL	0
#endif

#ifndef _SIZE_T_DEFINED
typedef unsigned int size_t;
#define _SIZE_T_DEFINED
#endif  /* _SIZE_T_DEFINED */




#endif

