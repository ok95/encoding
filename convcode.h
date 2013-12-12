
/*
    Project:
    File Name:      ConvCode.h
    Author:			ygy
    Date:
    C Compiler:    
    Purpose:        
    Copyright (C) 2009 Inventec Electronics (Nanjing) Co., Ltd.
    All rights reserved.
*/

#ifndef _CONVCODE_H_
#define _CONVCODE_H_

#ifndef _WORD
	typedef char				_CHAR;
	typedef unsigned char		_BYTE;
	typedef short				_INT;
	typedef unsigned short		_WORD;
	typedef int					_LONG;
	typedef unsigned int		_DWORD;
	typedef void				_VOID;
	typedef int					_BOOL;
#endif

#ifndef TRUE
	#define TRUE	1
#endif

#ifndef FALSE
	#define FALSE	0
#endif

#ifndef NULL
	#define NULL	0
#endif

///////////////////////////////////////////////////////

typedef enum AlphaIdCodingSchemeTag
{
	OCTET_ALIGNED_GSM_7BIT,
	DOUBLE_OCTET_UCS2 = 0x80,
	SINGLE_OCTET_BASE_POINTER_COMBINED,
	DOUBLE_OCTET_BASE_POINTER_COMBINED
}AlphaIdCodingScheme;


#define GSM_CHARACTER_SET_SIZE				0x80
#define NON_GSM								GSM_CHARACTER_SET_SIZE
#define UCS2_TO_GSM_LOOKUP_TABLE_SIZE		0x100
#define UCS2_GCL_RANGE						23

#define EXTENDED_ALPHABET_TABLE				0x1B
#define SPACE								0x20
#define GSM_GREEK_CAPITAL_LETTER_DELTA		0x10
#define UCS2_GREEK_CAPITAL_LETTER_GAMMA 	0x0393


typedef enum GsmCharTypeTag
{
	NON_GSM7BIT			= 0,
	GSM7BIT				= 1,
	EXTENDED_GSM7BIT	= 2
}GsmCharType;


GsmCharType ConvEGsmToUcs2(const _BYTE *gsmStr_p, _WORD gsmDataLength, _WORD *ucs2Str_p, _WORD *ucs2Length_p);
GsmCharType ConvUcs2ToEGsm(const _WORD *ucs2Str_p, _WORD ucs2Length, _BYTE *gsmStr_p, _WORD *gsmLength_p);
GsmCharType GetEGsmLengthOfUcs2(_WORD *ucs2Str_p, _WORD ucs2Length, _WORD *gsmLength_p);

GsmCharType ConvAsciiToEGsm(const _BYTE *asciiStr_p, _WORD asciiLength, _BYTE *gsmStr_p, _WORD *gsmLength_p);

_WORD ConvGsm7BitToChar( _BYTE *charString, const _BYTE *gsmData, _WORD length);
_WORD ConvUcs2ToChar( _BYTE *charStr, const _WORD *ucs2String, _WORD length);

_WORD LengthAlphaIdToUcs2( const _BYTE *alphaId, _WORD alphaIdLen );
_WORD ConvAlphaIdToUcs2( _WORD *strDst_p, _WORD maxDstLen, const _BYTE *alphaId, _WORD alphaIdLen );
_WORD ConvUcs2ToAlphaId ( _BYTE *alphaId_p, _WORD maxAlphaIdLen, const _WORD *strSrc_p, _WORD srcLen );

_WORD Encode7BitPackedData (_BYTE *buffer, _BYTE *msgData, _WORD msgLen, _WORD maxChars);
_WORD Decode7BitPackedData (_BYTE *destData, _WORD destLength, _BYTE *sourceData, _WORD sourceLength);

_VOID Bin2Str(_BYTE *pBcdData, _BYTE bcdDataLen, _BYTE *pTextStr);
_INT Str2Bin(_BYTE *pTextStr, _BYTE *pBinStr);

//////////////

#define LITTLE_ENDIAN		1234    /* least-significant byte first */
#define BIG_ENDIAN			4321    /* most-significant byte first */
#define PDP_ENDIAN			3412    /* LSB first in word, MSW first in long (pdp) */

_WORD ByteSwapWord(_WORD wVal, _WORD wByteOrder);
_DWORD ByteSwapDword(_DWORD dwVal, _WORD wByteOrder);

// only for big endian unicode string
_DWORD Unicode2ToUTF8(const _WORD *szUnicode, _DWORD dwUnicodeLen, _BYTE *szUtf8);
_DWORD UTF8ToUnicode2(const _BYTE *szUtf8, _DWORD dwUtf8Len, _WORD *szUnicode);

_DWORD Unicode4ToUTF8(const _DWORD *szUnicode, _DWORD dwUnicodeLen, _BYTE *szUtf8);
_DWORD UTF8ToUnicode4(const _BYTE *szUtf8, _DWORD dwUtf8Len, _DWORD *szUnicode);

_DWORD H80Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode);
_DWORD H81Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode);
_DWORD H82Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode);
_DWORD Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode);


//////////////////////////////////////////////////////








#endif

