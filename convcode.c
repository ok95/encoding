
/*
    Project:
    File Name:      ConvCode.c
    Author:			ygy
    Date:
    C Compiler:    
    Purpose:        
    Copyright (C) 2009 Inventec Electronics (Nanjing) Co., Ltd.
    All rights reserved.
*/

#include "convcode.h"
#include "string.h"


#ifndef _ASSERT
#define _ASSERT(x)		//while(1);
#endif



#ifdef WIN32
	#pragma warning(error: 4001) /**< nonstandard extension 'single line comment' was used (disallow "//" C++ comments) */
	#pragma warning(error: 4002) /**< too many actual parameters for macro 'identifier' */
	#pragma warning(error: 4003) /**< not enough actual parameters for macro 'identifier' */
	#pragma warning(error: 4004) /**< incorrect construction after 'defined' */
	#pragma warning(error: 4005) /**< 'identifier' : macro redefinition */
	#pragma warning(error: 4006) /**< #undef expected an identifier */
	#pragma warning(error: 4009) /**< string too big; trailing characters truncated */
	#pragma warning(error: 4013) /**< 'function' undefined; assuming extern returning int */
	#pragma warning(error: 4015) /**< 'identifier' : type of bit field must be integral */
	#pragma warning(error: 4016) /**< 'function' : no function return type; using int as default */
	#pragma warning(error: 4018) /**< 'expression' : signed/unsigned mismatch */
	#pragma warning(error: 4020) /**< 'function' : too many actual parameters */
	#pragma warning(error: 4021) /**< 'function' : too few actual parameters */
	#pragma warning(error: 4022) /**< 'function' : pointer mismatch for actual parameter 'number' */
	#pragma warning(error: 4023) /**< 'symbol' : based pointer passed to unprototyped function : parameter number */
	#pragma warning(error: 4024) /**< 'function' : different types for formal and actual parameter 'number' */
	#pragma warning(error: 4025) /**< 'number' : based pointer passed to function with variable arguments: parameter number */
	#pragma warning(error: 4026) /**< function declared with formal parameter list */
	#pragma warning(error: 4027) /**< function declared without formal parameter list */
	#pragma warning(error: 4028) /**< formal parameter 'number' different from declaration */
	#pragma warning(error: 4029) /**< declared formal parameter list different from definition */
	#pragma warning(error: 4030) /**< first formal parameter list longer than the second list */
	#pragma warning(error: 4031) /**< second formal parameter list longer than the first list */
	#pragma warning(error: 4033) /**< 'function' must return a value */
	#pragma warning(error: 4035) /**< 'function' : no return value */
	#pragma warning(error: 4036) /**< unnamed 'type' as actual parameter */
	#pragma warning(error: 4045) /**< 'identifier' : array bounds overflow */
	#pragma warning(error: 4047) /**< 'identifier1' : 'operator' : different levels of indirection from 'identifier2' */
	#pragma warning(error: 4049) /**< compiler limit : terminating line number emission */
	#pragma warning(error: 4051) /**< type conversion; possible loss of data */
	#pragma warning(error: 4053) /**< one void operand for '?:' */
	#pragma warning(error: 4054) /**< 'conversion' : from function pointer 'type1' to data pointer 'type2' */
	#pragma warning(error: 4057) /**< 'operator' : 'identifier1' indirection to slightly different base types from 'identifier2' */
	#pragma warning(error: 4059) /**< pascal string too big, length byte is length % 256 */
	#pragma warning(error: 4061) /**< enumerate 'identifier' in switch of enum 'identifier' is not explicitly handled by a case label */
	#pragma warning(error: 4063) /**< case 'identifier' is not a valid value for switch of enum 'identifier' */
	#pragma warning(error: 4064) /**< switch of incomplete enum 'identifier' */
	#pragma warning(error: 4071) /**< 'function' : no function prototype given */
	#pragma warning(error: 4072) /**< 'function' : no function prototype on 'convention' function */
	#pragma warning(error: 4078) /**< case constant 'value' too big for the type of the switch expression */
	#pragma warning(error: 4081) /**< expected 'token1'; found 'token2' */
	#pragma warning(error: 4087) /**< 'function' : declared with 'void' parameter list */
	#pragma warning(error: 4088) /**< 'function' : pointer mismatch in actual parameter 'number', formal parameter 'number' */
	#pragma warning(error: 4089) /**< 'function' : different types in actual parameter 'number', formal parameter 'number' */
	#pragma warning(error: 4098) /**< 'function' : void function returning a value */
	#pragma warning(error: 4113) /**< 'identifier1' differs in parameter lists from 'identifier2' */
	#pragma warning(error: 4129) /**< 'character' : unrecognized character escape sequence */
	#pragma warning(error: 4133) /**< 'type' : incompatible types - from 'type1' to 'type2' */
	#pragma warning(error: 4150) /**< deletion of pointer to incomplete type 'type'; no destructor called */
	#pragma warning(error: 4172) /**< returning address of local variable or temporary */
	#pragma warning(error: 4221) /**< nonstandard extension used : 'identifier' : cannot be initialized using address of automatic variable */
	#pragma warning(error: 4223) /**< nonstandard extension used : non-lvalue array converted to pointer */
	#pragma warning(error: 4224) /**< nonstandard extension used : formal parameter 'identifier' was previously defined as a type */
	#pragma warning(error: 4390) /**< ';' : empty controlled statement found; is this what was intended?" */
	#pragma warning(error: 4508) /**< 'function' : function should return a value; void return type assumed */
	#pragma warning(error: 4541) /**< 'identifier' used on polymorphic type 'type' with /GR-; unpredictable behavior may result */
	#pragma warning(error: 4551) /**< function call missing argument list */
	#pragma warning(error: 4553) /**< 'operator' : operator has no effect; did you intend 'operator'? */
	#pragma warning(error: 4700) /**< local variable 'name' used without having been initialized */
	#pragma warning(error: 4706) /**< assignment within conditional expression */
	#pragma warning(error: 4715) /**< 'function' : not all control paths return a value */
	#pragma warning(error: 4761) /**< integral size mismatch in argument : conversion supplied */
#endif /* WIN32 */
 
 

#define USE_CP1252_TABLE
//#define USE_CP858_TABLE
//#define USE_CP437_TABLE
//#define USE_ISO_8859_15_TABLE



///////////////////////////



/* The 0's in this table correspond exactly with the Greek character table definition GsmGclToUcs2 */
static const _WORD GsmToUcs2[GSM_CHARACTER_SET_SIZE] =
{      /*   +0x0     +0x1     +0x2     +0x3     +0x4     +0x5    +0x6    +0x7  */
/*0x00*/    0x40,    0xa3,    0x24,    0xa5,    0xe8,    0xe9,   0xf9,   0xec,
/*0x07*/    0xf2,    0xc7,    0x0a,    0xd8,    0xf8,    0x0d,   0xc5,   0xe5,
/*0x10*/       0,    0x5f,       0,       0,       0,       0,      0,      0,
/*0x18*/       0,       0,       0,    0x20,    0xc6,    0xe6,   0xdf,   0xc9,
/*0x20*/    0x20,    0x21,    0x22,    0x23,    0xA4,    0x25,   0x26,   0x27,
/*0x28*/    0x28,    0x29,    0x2a,    0x2b,    0x2c,    0x2d,   0x2e,   0x2f,
/*0x30*/    0x30,    0x31,    0x32,    0x33,    0x34,    0x35,   0x36,   0x37,
/*0x37*/    0x38,    0x39,    0x3a,    0x3b,    0x3c,    0x3d,   0x3e,   0x3f,
/*0x40*/    0xa1,    0x41,    0x42,    0x43,    0x44,    0x45,   0x46,   0x47,
/*0x48*/    0x48,    0x49,    0x4a,    0x4b,    0x4c,    0x4d,   0x4e,   0x4f,
/*0x50*/    0x50,    0x51,    0x52,    0x53,    0x54,    0x55,   0x56,   0x57,
/*0x58*/    0x58,    0x59,    0x5a,    0xc4,    0xd6,    0xd1,   0xdc,   0xa7,
/*0x60*/    0xbf,    0x61,    0x62,    0x63,    0x64,    0x65,   0x66,   0x67,
/*0x68*/    0x68,    0x69,    0x6a,    0x6b,    0x6c,    0x6d,   0x6e,   0x6f,
/*0x70*/    0x70,    0x71,    0x72,    0x73,    0x74,    0x75,   0x76,   0x77,
/*0x78*/    0x78,    0x79,    0x7a,    0xe4,    0xf6,    0xf1,   0xfc,   0xe0
};

/* array of UCS2 Greek character values correspondng to relevant GSM value - 0x10 - GSM value for Delta.
   The 0 at the second aray position corresponds to GSM _. */
static const _WORD GsmGclToUcs2[] =
{/*  Delta      --    Phi   Gamma   Lamda   Omega	*/
    0x0394, 0x0000, 0x03A6, 0x0393, 0x039b, 0x03a9,
/*      Pi     Psi   Sigma   Theta      Xi			*/
    0x03a0, 0x03a8, 0x03a3, 0x0398, 0x039e
};

/* GSM character and extended character values obtained from specification 23.038.
   UCS2 (unicode) values obtained from unicode specification version 3(www.unicode.org). */

static const _WORD GsmToUcs2Ex[GSM_CHARACTER_SET_SIZE] =
{      /*   +0x0    +0x1    +0x2    +0x3    +0x4    +0x5    +0x6    +0x7  */
/*0x00*/	0x40,	0xA3,	0x24, 	0xA5, 	0xE8, 	0xE9, 	0xF9, 	0xEC, 
/*0x08*/	0xF2, 	0xC7, 	0x0a, 	0xD8, 	0xF8, 	0x0d, 	0xC5, 	0xE5,
		/*  Delta      	    Phi    Gamma   Lamda   Omega	Pi     	Psi   */
/*0x10*/   0x394,	0x5F,  0x3A6,  0x393,  0x39B,  0x3A9,  0x3A0,  0x3A8,
		/*	Sigma   Theta   Xi	*/
/*0x18*/   0x3A3,  0x398,  0x39E,	0x20, 	0xC6, 	0xE6, 	0xDF, 	0xC9,
/*0x20*/    0x20, 	0x21, 	0x22, 	0x23, 	0xA4, 	0x25, 	0x26, 	0x27, 
/*0x28*/    0x28, 	0x29, 	0x2A, 	0x2B, 	0x2C, 	0x2D, 	0x2E, 	0x2F,
/*0x30*/    0x30, 	0x31, 	0x32, 	0x33, 	0x34, 	0x35, 	0x36, 	0x37, 
/*0x38*/    0x38, 	0x39, 	0x3A, 	0x3B, 	0x3C, 	0x3D, 	0x3E, 	0x3F,
/*0x40*/    0xA1, 	0x41, 	0x42, 	0x43, 	0x44, 	0x45, 	0x46, 	0x47, 
/*0x48*/    0x48, 	0x49, 	0x4A, 	0x4B, 	0x4C, 	0x4D, 	0x4E, 	0x4F,
/*0x50*/    0x50, 	0x51, 	0x52, 	0x53, 	0x54, 	0x55, 	0x56, 	0x57, 
/*0x58*/    0x58, 	0x59, 	0x5A, 	0xC4, 	0xD6, 	0xD1, 	0xDC, 	0xA7,
/*0x60*/    0xBF, 	0x61, 	0x62, 	0x63, 	0x64, 	0x65, 	0x66, 	0x67, 
/*0x68*/    0x68, 	0x69, 	0x6A, 	0x6B, 	0x6C, 	0x6D, 	0x6E, 	0x6F,
/*0x70*/    0x70, 	0x71, 	0x72, 	0x73, 	0x74, 	0x75, 	0x76, 	0x77, 
/*0x78*/    0x78, 	0x79, 	0x7A, 	0xE4, 	0xF6, 	0xF1, 	0xFC, 	0xE0
};

/* Extended Alphabet Table for Euro Character 0x20AC 
   Added SPACE 0x20 in table, at postion 27 (0x1B)
   so if we recieve to extended characters together we return a SPACE */
static const _WORD ExtendedGsmToUcs2[GSM_CHARACTER_SET_SIZE] =
{        /* +0x0     +0x1     +0x2    +0x3    +0x4     +0x5    +0x6    +0x7  */
/*0x00*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x08*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x10*/      0,       0,       0,      0,   0x5E,       0,      0,      0,		/* . . . . ^ . . . */
/*0x18*/      0,       0,       0,   0x20,      0,       0,      0,      0,		/* . . .   . . . . */
/*0x20*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x28*/   0x7B,    0x7D,       0,      0,      0,       0,      0,   0x5C,	    /* { } . . . . . \ */
/*0x30*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x38*/      0,       0,       0,      0,   0x5B,    0x7E,   0x5D,      0,		/* . . . . [ ~ ] . */
/*0x40*/   0x7C,       0,       0,      0,      0,       0,      0,      0,		/* | . . . . . . . */
/*0x48*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x50*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x58*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x60*/      0,       0,       0,      0,      0,  0x20AC,      0,      0,		/* . . . . . € . . */
/*0x68*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x70*/      0,       0,       0,      0,      0,       0,      0,      0,		/* . . . . . . . . */
/*0x78*/      0,       0,       0,      0,      0,       0,      0,      0		/* . . . . . . . . */
};


/*
  Convert a GSM or EGSM character to a ucs2 character.
 
  further reworking to protect the code after devAsserts so that a space is returned
  if the first GSM character value is out of range or a space is returned if the first character is
  ESC and the second character is out of range.
 
  Parameters:
  gsm1 - first character of EGSM pair (GSM value if not EGSM, ESC if EGSM)
  gsm2 - second character of EGSM pair (should be NON_GSM if not EGSM)
  ucs2 - pointer to the resultant ucs2 value.
 
  returns:
  GSM7BIT -          if normal GSM character
  NON_GSM7BIT -      if cannot be represented in GSM or EGSM
  EXTENDED_GSM7BIT - if EGSM character (pair)
 */
 
GsmCharType mapEGsmToUcs2(_BYTE gsm1, _BYTE gsm2, _WORD *ucs2_p)
{
	GsmCharType retVal = NON_GSM7BIT;

	_ASSERT(gsm1 < GSM_CHARACTER_SET_SIZE);

	/* If gsm1 is a valid GSM character then try to map it otherwise just return a SPACE */
	if (gsm1 < GSM_CHARACTER_SET_SIZE)
	{
		if (gsm1 == EXTENDED_ALPHABET_TABLE)	// 0x1B
		{
			/* Second Character NON_GSM - return Space */
			if(gsm2 == NON_GSM)
			{
				*ucs2_p = SPACE;
				retVal = GSM7BIT;
			}
			else
			{
				_ASSERT(gsm2 < GSM_CHARACTER_SET_SIZE);

				/* If gsm2 is a valid GSM character then try to map it otherwise just return a SPACE */
				if (gsm2 < GSM_CHARACTER_SET_SIZE)
				{
					*ucs2_p = ExtendedGsmToUcs2[gsm2];

					/* if no valid character found after escape character 0x1B,
						Need to display a space, and the character following the escape character
						is displayed using the default GsmToUcs2 Table */
					if(*ucs2_p == 0)
					{
						*ucs2_p = SPACE;
						retVal = GSM7BIT;
					}
					else
					{
						retVal = EXTENDED_GSM7BIT;
					}
				}
				else
				{
					/* gsm2 out of range - so just return a GSM7BIT space */
					*ucs2_p = SPACE;
					retVal = GSM7BIT;
				}
			}
		}
		else
		{
#if 1		
			*ucs2_p = GsmToUcs2[gsm1];

			/* if 0 returned then it must be a Greek character, as the GSMToUcs2 table is full apart 
			   from the Greek characters. 
				gsm1 must be in range as we've already checked that it's < GSM_CHARACTER_SET_SIZE. */
			if(*ucs2_p == 0)
			{
				*ucs2_p = GsmGclToUcs2[gsm1 - GSM_GREEK_CAPITAL_LETTER_DELTA];
			}
#else
			*ucs2_p = GsmToUcs2Ex[gsm1];
#endif
			retVal = GSM7BIT;
		}
	}
	else
	{
		/* gsm1 out of range - so just return a GSM7BIT space */
		*ucs2_p = SPACE;
		retVal = GSM7BIT;
	}

	return retVal;
}



/*
  Converts an array of bytes, each of which should be a valid GSM character (range 0 to 0x7F) to a 
  string of UCS2 characters.
  Stops when a conversion fails and returns the number of successful conversions performed.

  This function converts an array of bytes into UCS2. The bytes are ordered MSB,LSB,MSB,LSB.
*/

GsmCharType ConvEGsmToUcs2(const _BYTE *gsmStr_p, _WORD gsmDataLength, _WORD *ucs2Str_p, _WORD *ucs2Length_p)
{
	GsmCharType retVal 	 = GSM7BIT;
	GsmCharType charType = NON_GSM7BIT;
	_WORD ucs2Byte 	     = 0;
	_WORD currentGsm	 = 0;
	_WORD currentUcs2	 = 0;
	
	/* Loop through gsm data buffer upto last two characters */
	for(currentGsm = 0; currentGsm < gsmDataLength - 2; currentGsm++)
	{
		/* use mapEGsmToUcs2 to transform Gsm Characters into ucs2 character */
		charType = mapEGsmToUcs2(gsmStr_p[currentGsm], gsmStr_p[currentGsm+1], &ucs2Byte);

		if(charType == EXTENDED_GSM7BIT || charType == GSM7BIT)
		{
			/* store ucs2 Byte in Ucs2 String */
			ucs2Str_p[currentUcs2] = ucs2Byte;
			/*Increment Ucs2 String index */
			currentUcs2++;
			
			/* Extended Character Found */
			if(charType == EXTENDED_GSM7BIT)
      		{
				/* Increment currentGsm by 1 to move past Extended Character */
				currentGsm++;

				/* Set Return type */
		    	retVal = EXTENDED_GSM7BIT;
      		}
    	}
    	else
    	{
      		retVal = NON_GSM7BIT;
      		break;
    	}
  	}

	/* Check Valid GSM String */
	if((retVal == GSM7BIT) || ( retVal == EXTENDED_GSM7BIT) )
	{
		/* Check for two characters in Gsm Buffer */
		if(currentGsm < gsmDataLength - 1)
		{
			/* Use mapEGsmToUcs2 to convert last two characters in gsm String to Ucs2 */
			charType = mapEGsmToUcs2(gsmStr_p[currentGsm], gsmStr_p[currentGsm+1], &ucs2Byte);

			/* If last two characters were an extended Character */
			if(charType == EXTENDED_GSM7BIT)
			{
				/* store ucs2 Byte in Ucs2 String */
				ucs2Str_p[currentUcs2] = ucs2Byte;
				currentGsm+=2;
				/* Increment currentUcs2 for length of Buffer */
				currentUcs2++;
				
				/* Set return value to EXTENDED_GSM */
				retVal = EXTENDED_GSM7BIT;
			}
			/* Last Two Characters were not a extended character */
			else if(charType == GSM7BIT)
			{
				ucs2Str_p[currentUcs2] = ucs2Byte;
				currentUcs2++;
				currentGsm++;
			}
			else
			{
				retVal = NON_GSM7BIT;
			}
		}/* Not Enough Room in buffer to convert two characters */

		/* Check for One Character In Buffer, if there is one */
		if(currentGsm < gsmDataLength)
		{
			/* use mapEGsmToUcs2 to convert last character to Ucs2 */
			charType = mapEGsmToUcs2(gsmStr_p[currentGsm], NON_GSM, &ucs2Byte);

			/* Check Valid Byte was returned */
			if(charType == GSM7BIT)
			{
				/* insert last Ucs2 Byte into ucs2String */
				ucs2Str_p[currentUcs2] = ucs2Byte;
				/* Increment currentUcs2 for length of buffer */
				currentUcs2++;
				currentGsm++;
			}
			else
			{
				retVal = NON_GSM7BIT;
			}
		} /* Last Charactert in Buffer */

		/* Set Length of Ucs2 String */
		(*ucs2Length_p) = currentUcs2;

	} /* Invalid String */

	return retVal;
}


/////////////////////////////


static const _BYTE  Ucs2ToGsm[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] =
{          /*  +0x0       +0x1        +0x2        +0x3        +0x4        +0x5      +0x6        +0x7 */
/*0x00*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x08*/    NON_GSM,    NON_GSM,       0x0a,    NON_GSM,    NON_GSM,       0x0d,    NON_GSM,    NON_GSM,
/*0x10*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x18*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x20*/       0x20,       0x21,       0x22,       0x23,       0x02,       0x25,       0x26,       0x27,
/*0x28*/       0x28,       0x29,       0x2a,       0x2b,       0x2c,       0x2d,       0x2e,       0x2f,
/*0x30*/       0x30,       0x31,       0x32,       0x33,       0x34,       0x35,       0x36,       0x37,
/*0x38*/       0x38,       0x39,       0x3a,       0x3b,       0x3c,       0x3d,       0x3e,       0x3f,
/*0x40*/       0x00,       0x41,       0x42,       0x43,       0x44,       0x45,       0x46,       0x47,
/*0x48*/       0x48,       0x49,       0x4a,       0x4b,       0x4c,       0x4d,       0x4e,       0x4f,
/*0x50*/       0x50,       0x51,       0x52,       0x53,       0x54,       0x55,       0x56,       0x57,
/*0x58*/       0x58,       0x59,       0x5a,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0x11,
/*0x60*/    NON_GSM,       0x61,       0x62,       0x63,       0x64,       0x65,       0x66,       0x67,
/*0x68*/       0x68,       0x69,       0x6a,       0x6b,       0x6c,       0x6d,       0x6e,       0x6f,
/*0x70*/       0x70,       0x71,       0x72,       0x73,       0x74,       0x75,       0x76,       0x77,
/*0x78*/       0x78,       0x79,       0x7a,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x80*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x88*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x90*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x98*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0xa0*/    NON_GSM,       0x40,    NON_GSM,       0x01,       0x24,       0x03,    NON_GSM,       0x5f,
/*0xa8*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0xb0*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0xb8*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0x60,
/*0xc0*/    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0x5b,       0x0e,       0x1c,       0x09,
/*0xc8*/    NON_GSM,       0x1f,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0x60,
/*0xd0*/    NON_GSM,       0x5d,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0x5c,    NON_GSM,
/*0xd8*/       0x0b,    NON_GSM,    NON_GSM,    NON_GSM,       0x5e,    NON_GSM,    NON_GSM,       0x1e,
/*0xe0*/       0x7f,    NON_GSM,    NON_GSM,    NON_GSM,       0x7b,       0x0f,       0x1d,    NON_GSM,
/*0xe8*/       0x04,       0x05,    NON_GSM,    NON_GSM,       0x07,    NON_GSM,    NON_GSM,    NON_GSM,
/*0xf0*/    NON_GSM,       0x7d,       0x08,    NON_GSM,    NON_GSM,    NON_GSM,       0x7c,    NON_GSM,
/*0xf8*/       0x0c,       0x06,    NON_GSM,    NON_GSM,       0x7e,    NON_GSM,    NON_GSM,    NON_GSM
};


static const _BYTE Ucs2GclToGsm[UCS2_GCL_RANGE + 1] =
{
/*0x0393*/	   0x13,	   0x10,	NON_GSM,	NON_GSM, 
/*0x0397*/	NON_GSM,	   0x19,	NON_GSM,	NON_GSM,
/*0x039b*/	   0x14,	NON_GSM,	NON_GSM,	   0x1a,
/*0x039f*/	NON_GSM,	   0x16,	NON_GSM,	NON_GSM,
/*0x03a3*/	   0x18,	NON_GSM,	NON_GSM,	   0x12,
/*0x03a7*/	NON_GSM,	   0x17,	   0x15,	   0x16
};


/*
  Convert a ucs2 character value to an extended GSM character.
 
  further reworking so that if the UCS2 value is less than the maximum GSM
  character value the extended table is searched.
 
  Parameters:
  ucs2 -   the ucs2 value to convert to EGSM.
  gsm1_p - pointer to first character of EGSM pair (GSM value if not EGSM, ESC if EGSM)
  gsm2_p - pointer to second character of EGSM pair (NON_GSM if not EGSM)
 
  returns:
  GSM7BIT -          if normal GSM character
  NON_GSM7BIT -      if cannot be represented in GSM or EGSM
  EXTENDED_GSM7BIT - if EGSM character (pair)
 */
 
GsmCharType mapUcs2ToEGsm(_WORD ucs2, _BYTE *gsm1_p, _BYTE *gsm2_p)
{
	GsmCharType		retVal   = NON_GSM7BIT;
	_BYTE			gsmValue = NON_GSM;

	/* Check to see whether the UCS2 character is represented by a single GSM character.
		This can be either a 'normal' character or a Greek character. */

	if(gsm1_p != NULL && gsm2_p != NULL)
	{
		if(ucs2 < UCS2_TO_GSM_LOOKUP_TABLE_SIZE)
		{
			gsmValue = Ucs2ToGsm[ucs2];

			*gsm1_p = gsmValue;
			*gsm2_p = NON_GSM;

			if (gsmValue != NON_GSM)
			{
				retVal = GSM7BIT;
			}
		}
		else if((ucs2 >= UCS2_GREEK_CAPITAL_LETTER_GAMMA) &&
				(ucs2 <= (UCS2_GREEK_CAPITAL_LETTER_GAMMA + UCS2_GCL_RANGE)))
		{
			gsmValue = Ucs2GclToGsm[(ucs2) - UCS2_GREEK_CAPITAL_LETTER_GAMMA];

			*gsm1_p = gsmValue;
			*gsm2_p = NON_GSM;

			if (gsmValue != NON_GSM)
			{
				retVal = GSM7BIT;
			}
		}

		/* It's not a single GSM character, so if we want to send it as unicode do nothing,
			otherwise try and find the character in the extended GSM table. */

		if ( gsmValue == NON_GSM)
		{
			_BOOL found = FALSE;
			_BYTE count = 0;

			while((count < GSM_CHARACTER_SET_SIZE) && (found == FALSE))
			{
				if(ExtendedGsmToUcs2[count] == ucs2)
				{
					found = TRUE;
				}
				else
				{
					count++;
				}
			}

			/* If Ucs2 Character is found in Table insert characters in gsm data buffers
			   First character is Extended_Alphabet_Table Value (0x1B) to indicate Extended Gsm Character.
			   Second Gsm character is value returned from loop.
			   Indicate in return value it is an extended character. */
			if (found)
			{
				*gsm1_p = EXTENDED_ALPHABET_TABLE;

				*gsm2_p = count;

				retVal = EXTENDED_GSM7BIT;
			}
		}

	} /* gsm1_p and gsm2 != NULL */

	return retVal;
}


/*
  Converts a Ucs2 string to byte aligned GSM.
  Stops when a conversion fails and returns the number of successful conversions performed.
  If the destination gsmData equals NULL, don't store the conversion result.

  - gsmData_p pointer to gsm string buffer
  - ucs2String_p pointer to ucs2 string buffer
  - length size of ucs2 string.
  - Offset added to take into consideration the extra character
    needed for the Exteneded Characters. It is increment by one
    every exteneded character so the extra length can be taken into
    consideration. e.g. Length = LengthUcs2 + Offset.
  - current used to itterate through ucs2 string
 */

GsmCharType ConvUcs2ToEGsm(const _WORD *ucs2Str_p, _WORD ucs2Length, _BYTE *gsmStr_p, _WORD *gsmLength_p)
{
	GsmCharType retVal	 = GSM7BIT;
	GsmCharType charType = NON_GSM7BIT;
	_WORD currentGsm 	 = 0;
	_WORD currentUcs2    = 0;
	_BYTE	gsmChar1;
	_BYTE 	gsmChar2;
	
	/* Initialise gsm length */
	*gsmLength_p = 0;

	/* Check for valid gsmStr_p */
	if(gsmStr_p != NULL)
	{
		/* Loop through ucs2 String */
		for(currentUcs2 = 0; currentUcs2 < ucs2Length; currentUcs2++)
		{
			/* use mapUcs2ToEGsm to map ucs2 to gsmCharacters */
			charType = mapUcs2ToEGsm(ucs2Str_p[currentUcs2], &gsmChar1, &gsmChar2);

			/* If ucs2 character mapped to an Extended Character */
			if(charType == EXTENDED_GSM7BIT)
			{
				/* Set gsmStr to both characters returned in gsmChar1, and gsmChar2 */
				gsmStr_p[currentGsm]    = gsmChar1;
				gsmStr_p[currentGsm+1] 	= gsmChar2;

				/* Increment Length by 2 for Extended Character */
				(*gsmLength_p)+=2;
				/* Increment currentGsm by 2 to avoid Extended Characters */
				currentGsm+=2;

				retVal = EXTENDED_GSM7BIT;
			}
			/* If ucs2 character mapped to normal Gsm Character */
			else if(charType == GSM7BIT)
			{
				/* Store Gsm Character in gsm string */
				gsmStr_p[currentGsm] = gsmChar1;
				/* Increment Length by 1 */
				(*gsmLength_p)++;
				/* Increment current Gsm by 1 */
				currentGsm++;
			}
			else
			{
				/* Invalid Character Found set retVal to NON_GSM*/
				retVal = NON_GSM7BIT;
				break;
			}
		}
	}
	else
	{
		/* No Valid Gsm Data Buffer return NON_GSM */
		retVal = NON_GSM7BIT;
	}

	return retVal;
}



/* 
  This function returns the length of the Gsm String as
  now with extended Characters we cannot assume a Ucs2
  Character is equal to one Byte
  - gsmData_p pointer to the buffer for the GsmData can be Null
  - ucs2String_p pointer to the Ucs2 String being converted to Gsm
  - length length of Ucs2String
 */
 GsmCharType GetEGsmLengthOfUcs2(_WORD *ucs2Str_p, _WORD ucs2Length, _WORD *gsmLength_p)
{
	GsmCharType retVal		= GSM7BIT;
	GsmCharType charType	= NON_GSM7BIT;
	_WORD	currentLen		= 0;
	_BYTE	gsmChar1		= 0;
	_BYTE	gsmChar2		= 0;

	/* Initialise gsm Length to 0 */
	*gsmLength_p = 0;

	/* Loop through Ucs2 String */
	while(currentLen < ucs2Length)
	{
		/* Use mapUcs2ToGsm To determine if ucs2 character can be converted */
		charType = mapUcs2ToEGsm(ucs2Str_p[currentLen], &gsmChar1, &gsmChar2);

		/* If Ucs2 maps to Extended Character */
		if(charType == EXTENDED_GSM7BIT)
		{
			/* Increment gsmLength by two for the Extended Character */
			(*gsmLength_p)+=2;

			retVal = EXTENDED_GSM7BIT;
		}
		/* If Ucs2 maps To Normal Gsm Character */
		else if(charType == GSM7BIT)
		{
			/* Increment gsmLength by 1 for Gsm Character */
			(*gsmLength_p)++;
		}
		else
		{
			retVal = NON_GSM7BIT;
			/* Non Gsm break out of loop */
			break;
		}

		/* Increment currentLen to index next Ucs2 Character */
		currentLen++;
	}

	return retVal;
}



/////////////////////////////


/* code page 437 */
static const _WORD cp437_table[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] =
{       /*    +0x0       +0x1       +0x2       +0x3       +0x4       +0x5       +0x6       +0x7   */
/*0x00*/    0x2007,    0x253a,    0x253b,    0x2665,    0x2666,    0x2663,    0x2660,    0x2022,
/*0x08*/    0x25d8,    0x25cb,    0x25d9,    0x2642,    0x2640,    0x266a,    0x266b,    0x263c,
/*0x10*/    0x25ba,    0x25c4,    0x2195,    0x203c,    0x00b6,    0x00a7,    0x25ac,    0x21a8,
/*0x18*/    0x2191,    0x2193,    0x2192,    0x2190,    0x221f,    0x2194,    0x25b2,    0x25bc,
/*0x20*/      0x20,      0x21,      0x22,      0x23,      0x24,      0x25,      0x26,      0x27,
/*0x28*/      0x28,      0x29,      0x2a,      0x2b,      0x2c,      0x2d,      0x2e,      0x2f,
/*0x30*/      0x30,      0x31,      0x32,      0x33,      0x34,      0x35,      0x36,      0x37,
/*0x38*/      0x38,      0x39,      0x3a,      0x3b,      0x3c,      0x3d,      0x3e,      0x3f,
/*0x40*/      0x40,      0x41,      0x42,      0x43,      0x44,      0x45,      0x46,      0x47,
/*0x48*/      0x48,      0x49,      0x4a,      0x4b,      0x4c,      0x4d,      0x4e,      0x4f,
/*0x50*/      0x50,      0x51,      0x52,      0x53,      0x54,      0x55,      0x56,      0x57,
/*0x58*/      0x58,      0x59,      0x5a,      0x5b,      0x5c,      0x5d,      0x5e,      0x5f,
/*0x60*/      0x60,      0x61,      0x62,      0x63,      0x64,      0x65,      0x66,      0x67,
/*0x68*/      0x68,      0x69,      0x6a,      0x6b,      0x6c,      0x6d,      0x6e,      0x6f,
/*0x70*/      0x70,      0x71,      0x72,      0x73,      0x74,      0x75,      0x76,      0x77,
/*0x78*/      0x78,      0x79,      0x7a,      0x7b,      0x7c,      0x7d,      0x7e,    0x2302,
/*0x80*/      0xc7,      0xfc,      0xe9,      0xe2,      0xe4,      0xe0,      0xe5,      0xe7,
/*0x88*/      0xea,      0xeb,      0xe8,      0xef,      0xee,      0xec,      0xc4,      0xc5,
/*0x90*/      0xc9,      0xe5,      0xc6,      0xf4,      0xf6,      0xf2,      0xfb,      0xf9,
/*0x98*/      0xff,      0xd6,      0xdc,      0xa2,      0xa3,      0xa5,      0xa7,    0x0192,
/*0xa0*/      0xe1,      0xed,      0xf3,      0xfa,      0xf1,      0xd1,      0xaa,      0xba,
/*0xa8*/      0xbf,    0x2310,      0xac,      0xbd,      0xbc,      0xa1,      0xab,      0xbb,
/*0xb0*/    0x2591,    0x2592,    0x2593,    0x2502,    0x2524,    0x2561,    0x2562,    0x2556,
/*0xb8*/    0x2555,    0x2563,    0x2551,    0x2557,    0x255d,    0x255c,    0x255b,    0x2510,
/*0xc0*/    0x2514,    0x2534,    0x252c,    0x251c,    0x2500,    0x253c,    0x255e,    0x255f,
/*0xc8*/    0x255a,    0x2554,    0x2569,    0x2566,    0x2560,    0x2550,    0x256c,    0x2567,
/*0xd0*/    0x2568,    0x2564,    0x2565,    0x2559,    0x2558,    0x2552,    0x2553,    0x256b,
/*0xd8*/    0x256a,    0x2518,    0x250c,    0x2588,    0x2584,    0x258c,    0x2590,    0x2580,
/*0xe0*/    0x03b1,    0x03b2,    0x0393,    0x03c0,    0x03a3,    0x03c3,      0xb5,    0x03c4,
/*0xe8*/    0x03a6,    0x0398,    0x03a9,    0x03b4,    0x221e,    0x2205,    0x2208,    0x2229,
/*0xf0*/    0x2261,      0xb1,    0x2265,    0x2264,    0x2320,    0x2321,      0xf7,    0x2248,
/*0xf8*/      0xb0,    0x2219,      0xb7,    0x221a,    0x207f,      0xb2,    0x25a0,      0xa0
};



/* A modification of CP850 is CP858, differing only in the character at position 0xD5: 
    a dotless-i (0x0131) in CP850, which is replaced with a euro sign (0x20ac) in CP858.*/
/* code page 858 */
static const _WORD cp858_table[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] = 
{      /*     +0x0       +0x1       +0x2       +0x3       +0x4       +0x5       +0x6       +0x7   */
/*0x00*/    0x2007,    0x253a,    0x253b,    0x2665,    0x2666,    0x2663,    0x2660,    0x2022,
/*0x08*/    0x25d8,    0x25cb,    0x25d9,    0x2642,    0x2640,    0x266a,    0x266b,    0x263c,
/*0x10*/    0x25ba,    0x25c4,    0x2195,    0x203c,    0x00b6,    0x00a7,    0x25ac,    0x21a8,
/*0x18*/    0x2191,    0x2193,    0x2192,    0x2190,    0x221f,    0x2194,    0x25b2,    0x25bc,
/*0x20*/      0x20,      0x21,      0x22,      0x23,      0x24,      0x25,      0x26,      0x27,
/*0x28*/      0x28,      0x29,      0x2a,      0x2b,      0x2c,      0x2d,      0x2e,      0x2f,
/*0x30*/      0x30,      0x31,      0x32,      0x33,      0x34,      0x35,      0x36,      0x37,
/*0x38*/      0x38,      0x39,      0x3a,      0x3b,      0x3c,      0x3d,      0x3e,      0x3f,
/*0x40*/      0x40,      0x41,      0x42,      0x43,      0x44,      0x45,      0x46,      0x47,
/*0x48*/      0x48,      0x49,      0x4a,      0x4b,      0x4c,      0x4d,      0x4e,      0x4f,
/*0x50*/      0x50,      0x51,      0x52,      0x53,      0x54,      0x55,      0x56,      0x57,
/*0x58*/      0x58,      0x59,      0x5a,      0x5b,      0x5c,      0x5d,      0x5e,      0x5f,
/*0x60*/      0x60,      0x61,      0x62,      0x63,      0x64,      0x65,      0x66,      0x67,
/*0x68*/      0x68,      0x69,      0x6a,      0x6b,      0x6c,      0x6d,      0x6e,      0x6f,
/*0x70*/      0x70,      0x71,      0x72,      0x73,      0x74,      0x75,      0x76,      0x77,
/*0x78*/      0x78,      0x79,      0x7a,      0x7b,      0x7c,      0x7d,      0x7e,    0x2302,
/*0x80*/      0xc7,      0xfc,      0xe9,      0xe2,      0xe4,      0xe0,      0xe5,      0xe7,
/*0x88*/      0xea,      0xeb,      0xe8,      0xef,      0xee,      0xec,      0xc4,      0xc5,
/*0x90*/      0xc9,      0xe6,      0xc6,      0xf4,      0xf6,      0xf2,      0xfb,      0xf9,
/*0x98*/      0xff,      0xd6,      0xdc,      0xf8,      0xa3,      0xd8,      0xd7,    0x0192,
/*0xa0*/      0xe1,      0xed,      0xf3,      0xfa,      0xf1,      0xd1,      0xaa,      0xba,
/*0xa8*/      0xbf,      0xae,      0xac,      0xbd,      0xbc,      0xa1,      0xab,      0xbb,
/*0xb0*/    0x2591,    0x2592,    0x2593,    0x2502,    0x2524,      0xc1,      0xc2,      0xc0,
/*0xb8*/      0xa9,    0x2563,    0x2551,    0x2557,    0x255d,      0xa2,      0xa5,    0x2510,
/*0xc0*/    0x2514,    0x2534,    0x252c,    0x251c,    0x2500,    0x253c,      0xe3,      0xc3,
/*0xc8*/    0x255a,    0x2554,    0x2569,    0x2566,    0x2560,    0x2550,    0x256c,      0xa4,
/*0xd0*/      0xf0,      0xd0,      0xca,      0xcb,      0xc8,    0x20ac,      0xcd,      0xce,
/*0xd8*/      0xcf,    0x2518,    0x250c,    0x2588,    0x2584,      0xa5,      0xcc,    0x2580,
/*0xe0*/      0xd3,      0xdf,      0xd4,      0xd2,      0xf5,      0xd5,      0xb5,      0xfe,
/*0xe8*/      0xde,      0xda,      0xdb,      0xd9,      0xfd,      0xdd,      0xaf,      0xb4,
/*0xf0*/      0xad,      0xb1,    0x2017,      0xbe,      0xb6,      0xa7,      0xf7,      0xb8,
/*0xf8*/      0xb0,      0xa8,      0xb7,      0xb9,      0xb3,      0xb2,    0x25a0,      0xa0
};


/* cp1252, ansi */	
static const _WORD cp1252_table[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] = 
{    /*       +0x0     +0x1     +0x2     +0x3     +0x4     +0x5     +0x6     +0x7   */
/*0x00*/      0x00,    0x01,    0x02,    0x03,    0x04,    0x05,    0x06,    0x07,
/*0x08*/      0x08,    0x09,    0x0a,    0x0b,    0x0c,    0x0d,    0x0e,    0x0f,
/*0x10*/      0x10,    0x11,    0x12,    0x13,    0x14,    0x15,    0x16,    0x17,
/*0x18*/      0x18,    0x19,    0x1a,    0x1b,    0x1c,    0x1d,    0x1e,    0x1f,
/*0x20*/      0x20,    0x21,    0x22,    0x23,    0x24,    0x25,    0x26,    0x27,
/*0x28*/      0x28,    0x29,    0x2a,    0x2b,    0x2c,    0x2d,    0x2e,    0x2f,
/*0x30*/      0x30,    0x31,    0x32,    0x33,    0x34,    0x35,    0x36,    0x37,
/*0x38*/      0x38,    0x39,    0x3a,    0x3b,    0x3c,    0x3d,    0x3e,    0x3f,
/*0x40*/      0x40,    0x41,    0x42,    0x43,    0x44,    0x45,    0x46,    0x47,
/*0x48*/      0x48,    0x49,    0x4a,    0x4b,    0x4c,    0x4d,    0x4e,    0x4f,
/*0x50*/      0x50,    0x51,    0x52,    0x53,    0x54,    0x55,    0x56,    0x57,
/*0x58*/      0x58,    0x59,    0x5a,    0x5b,    0x5c,    0x5d,    0x5e,    0x5f,
/*0x60*/      0x60,    0x61,    0x62,    0x63,    0x64,    0x65,    0x66,    0x67,
/*0x68*/      0x68,    0x69,    0x6a,    0x6b,    0x6c,    0x6d,    0x6e,    0x6f,
/*0x70*/      0x70,    0x71,    0x72,    0x73,    0x74,    0x75,    0x76,    0x77,
/*0x78*/      0x78,    0x79,    0x7a,    0x7b,    0x7c,    0x7d,    0x7e,    0x7f,
/*0x80*/    0x20ac,       0,  0x201a,  0x0192,  0x201e,  0x2026,  0x2020,  0x2021,
/*0x88*/    0x02c6,  0x2030,  0x0160,  0x2039,  0x0152,       0,  0x017d,       0,
/*0x90*/         0,  0x2018,  0x2019,  0x201c,  0x201d,  0x2022,  0x2013,  0x2014,
/*0x98*/    0x02dc,  0x2122,  0x0161,  0x203a,  0x0153,       0,  0x017e,  0x0178,
/*0xa0*/      0xa0,    0xa1,    0xa2,    0xa3,    0xa4,    0xa5,    0xa6,    0xa7,
/*0xa8*/      0xa8,    0xa9,    0xaa,    0xab,    0xac,    0xad,    0xae,    0xaf,
/*0xb0*/      0xb0,    0xb1,    0xb2,    0xb3,    0xb4,    0xb5,    0xb6,    0xb7,
/*0xb8*/      0xb8,    0xb9,    0xba,    0xbb,    0xbc,    0xbd,    0xbe,    0xbf,
/*0xc0*/      0xc0,    0xc1,    0xc2,    0xc3,    0xc4,    0xc5,    0xc6,    0xc7,
/*0xc8*/      0xc8,    0xc9,    0xca,    0xcb,    0xcc,    0xcd,    0xce,    0xcf,
/*0xd0*/      0xd0,    0xd1,    0xd2,    0xd3,    0xd4,    0xd5,    0xd6,    0xd7,
/*0xd8*/      0xd8,    0xd9,    0xda,    0xdb,    0xdc,    0xdd,    0xde,    0xdf,
/*0xe0*/      0xe0,    0xe1,    0xe2,    0xe3,    0xe4,    0xe5,    0xe6,    0xe7,
/*0xe8*/      0xe8,    0xe9,    0xea,    0xeb,    0xec,    0xed,    0xee,    0xef,
/*0xf0*/      0xf0,    0xf1,    0xf2,    0xf3,    0xf4,    0xf5,    0xf6,    0xf7,
/*0xf8*/      0xf8,    0xf9,    0xfa,    0xfb,    0xfc,    0xfd,    0xfe,    0xff
};

	
/* iso-8859-15 */	
static const _WORD ISO_8859_15_table[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] = 
{      /*     +0x0     +0x1     +0x2     +0x3     +0x4      +0x5      +0x6      +0x7   */
/*0x00*/      0x00,    0x01,    0x02,    0x03,    0x04,     0x05,     0x06,     0x07,
/*0x08*/      0x08,    0x09,    0x0a,    0x0b,    0x0c,     0x0d,     0x0e,     0x0f,
/*0x10*/      0x10,    0x11,    0x12,    0x13,    0x14,     0x15,     0x16,     0x17,
/*0x18*/      0x18,    0x19,    0x1a,    0x1b,    0x1c,     0x1d,     0x1e,     0x1f,
/*0x20*/      0x20,    0x21,    0x22,    0x23,    0x24,     0x25,     0x26,     0x27,
/*0x28*/      0x28,    0x29,    0x2a,    0x2b,    0x2c,     0x2d,     0x2e,     0x2f,
/*0x30*/      0x30,    0x31,    0x32,    0x33,    0x34,     0x35,     0x36,     0x37,
/*0x38*/      0x38,    0x39,    0x3a,    0x3b,    0x3c,     0x3d,     0x3e,     0x3f,
/*0x40*/      0x40,    0x41,    0x42,    0x43,    0x44,     0x45,     0x46,     0x47,
/*0x48*/      0x48,    0x49,    0x4a,    0x4b,    0x4c,     0x4d,     0x4e,     0x4f,
/*0x50*/      0x50,    0x51,    0x52,    0x53,    0x54,     0x55,     0x56,     0x57,
/*0x58*/      0x58,    0x59,    0x5a,    0x5b,    0x5c,     0x5d,     0x5e,     0x5f,
/*0x60*/      0x60,    0x61,    0x62,    0x63,    0x64,     0x65,     0x66,     0x67,
/*0x68*/      0x68,    0x69,    0x6a,    0x6b,    0x6c,     0x6d,     0x6e,     0x6f,
/*0x70*/      0x70,    0x71,    0x72,    0x73,    0x74,     0x75,     0x76,     0x77,
/*0x78*/      0x78,    0x79,    0x7a,    0x7b,    0x7c,     0x7d,     0x7e,     0x7f,
/*0x80*/      0x80,    0x81,    0x82,    0x83,    0x84,     0x85,     0x86,     0x87,
/*0x88*/      0x88,    0x89,    0x8a,    0x8b,    0x8c,     0x8d,     0x8e,     0x8f,
/*0x90*/      0x90,    0x91,    0x92,    0x93,    0x94,     0x95,     0x96,     0x97,
/*0x98*/      0x98,    0x99,    0x9a,    0x9b,    0x9c,     0x9d,     0x9e,     0x9f,
/*0xa0*/      0xa0,    0xa1,    0xa2,    0xa3,  0x20ac,     0xa5,   0x0160,     0xa7,
/*0xa8*/    0x0161,    0xa9,    0xaa,    0xab,    0xac,     0xad,     0xae,     0xaf,
/*0xb0*/      0xb0,    0xb1,    0xb2,    0xb3,  0x017d,     0xb5,     0xb6,     0xb7,
/*0xb8*/    0x017e,    0xb9,    0xba,    0xbb,  0x0152,   0x0153,   0x0178,     0xbf,
/*0xc0*/      0xc0,    0xc1,    0xc2,    0xc3,    0xc4,     0xc5,     0xc6,     0xc7,
/*0xc8*/      0xc8,    0xc9,    0xca,    0xcb,    0xcc,     0xcd,     0xce,     0xcf,
/*0xd0*/      0xd0,    0xd1,    0xd2,    0xd3,    0xd4,     0xd5,     0xd6,     0xd7,
/*0xd8*/      0xd8,    0xd9,    0xda,    0xdb,    0xdc,     0xdd,     0xde,     0xdf,
/*0xe0*/      0xe0,    0xe1,    0xe2,    0xe3,    0xe4,     0xe5,     0xe6,     0xe7,
/*0xe8*/      0xe8,    0xe9,    0xea,    0xeb,    0xec,     0xed,     0xee,     0xef,
/*0xf0*/      0xf0,    0xf1,    0xf2,    0xf3,    0xf4,     0xf5,     0xf6,     0xf7,
/*0xf8*/      0xf8,    0xf9,    0xfa,    0xfb,    0xfc,     0xfd,     0xfe,     0xff
};


//////////////////


static const _BYTE GsmToCP437[GSM_CHARACTER_SET_SIZE] =
{         /*   +0x0     +0x1     +0x2     +0x3    +0x4     +0x5    +0x6    +0x7  */
/*0x00*/    0x40,    0x9c,    0x24,    0x9d,    0x8a,    0x82,    0x97,    0x8d,
/*0x08*/    0x95,    0x80,       0,      0,        0,       0,    0x8f,    0x86,
/*0x10*/       0,    0x5f,    0xe8,    0xe2,       0,    0xea,       0,       0,
/*0x18*/    0xe4,    0xe9,       0,    0x20,    0x92,       0,       0,    0x90,
/*0x20*/    0x20,    0x21,    0x22,    0x23,       0,    0x25,    0x26,    0x27,
/*0x28*/    0x28,    0x29,    0x2a,    0x2b,    0x2c,    0x2d,    0x2e,    0x2f,
/*0x30*/    0x30,    0x31,    0x32,    0x33,    0x34,    0x35,    0x36,    0x37,
/*0x38*/    0x38,    0x39,    0x3a,    0x3b,    0x3c,    0x3d,    0x3e,    0x3f,
/*0x40*/    0xad,    0x41,    0x42,    0x43,    0x44,    0x45,    0x46,    0x47,
/*0x48*/    0x48,    0x49,    0x4a,    0x4b,    0x4c,    0x4d,    0x4e,    0x4f,
/*0x50*/    0x50,    0x51,    0x52,    0x53,    0x54,    0x55,    0x56,    0x57,
/*0x58*/    0x58,    0x59,    0x5a,    0x8e,    0x99,    0xa5,    0x9a,    0x15,
/*0x60*/    0xa8,    0x61,    0x62,    0x63,    0x64,    0x65,    0x66,    0x67,
/*0x68*/    0x68,    0x69,    0x6a,    0x6b,    0x6c,    0x6d,    0x6e,    0x6f,
/*0x70*/    0x70,    0x71,    0x72,    0x73,    0x74,    0x75,    0x76,    0x77,
/*0x78*/    0x78,    0x79,    0x7a,    0x84,    0x94,    0xa4,    0x81,    0x85,
};


static const _BYTE GsmToCP858[GSM_CHARACTER_SET_SIZE] =
{         /*   +0x0     +0x1     +0x2     +0x3    +0x4     +0x5    +0x6    +0x7  */
/*0x00*/    0x40,    0x9c,    0x24,    0xbe,    0x8a,    0x82,    0x97,    0x8d,
/*0x08*/    0x95,    0x80,       0,    0x9d,    0x9b,       0,    0x8f,    0x86,
/*0x10*/       0,    0x5f,       0,       0,       0,       0,       0,       0,
/*0x18*/       0,       0,       0,    0x20,    0x92,    0x91,    0xe1,    0x90,
/*0x20*/    0x20,    0x21,    0x22,    0x23,    0xcf,    0x25,    0x26,    0x27,
/*0x28*/    0x28,    0x29,    0x2a,    0x2b,    0x2c,    0x2d,    0x2e,    0x2f,
/*0x30*/    0x30,    0x31,    0x32,    0x33,    0x34,    0x35,    0x36,    0x37,
/*0x38*/    0x38,    0x39,    0x3a,    0x3b,    0x3c,    0x3d,    0x3e,    0x3f,
/*0x40*/    0xad,    0x41,    0x42,    0x43,    0x44,    0x45,    0x46,    0x47,
/*0x48*/    0x48,    0x49,    0x4a,    0x4b,    0x4c,    0x4d,    0x4e,    0x4f,
/*0x50*/    0x50,    0x51,    0x52,    0x53,    0x54,    0x55,    0x56,    0x57,
/*0x58*/    0x58,    0x59,    0x5a,    0x8e,    0x99,    0xa5,    0x9a,    0x15,
/*0x60*/    0xa8,    0x61,    0x62,    0x63,    0x64,    0x65,    0x66,    0x67,
/*0x68*/    0x68,    0x69,    0x6a,    0x6b,    0x6c,    0x6d,    0x6e,    0x6f,
/*0x70*/    0x70,    0x71,    0x72,    0x73,    0x74,    0x75,    0x76,    0x77,
/*0x78*/    0x78,    0x79,    0x7a,    0x84,    0x94,    0xa4,    0x81,    0x85,
};


static const _BYTE GsmToCP1252[GSM_CHARACTER_SET_SIZE] =
{         /*   +0x0     +0x1     +0x2     +0x3    +0x4     +0x5    +0x6    +0x7  */
/*0x00*/    0x40,    0xa3,    0x24,    0xa5,    0xe8,    0xe9,    0xf9,    0xec,
/*0x08*/    0xf2,    0xc7,    0x0a,    0xd8,    0xf8,    0x0d,    0xc5,    0xe5,
/*0x10*/       0,    0x5f,       0,       0,       0,       0,       0,       0,
/*0x18*/       0,       0,       0,    0x20,    0xc6,    0xe6,    0xdf,    0xc9,
/*0x20*/    0x20,    0x21,    0x22,    0x23,    0xa4,    0x25,    0x26,    0x27,
/*0x28*/    0x28,    0x29,    0x2a,    0x2b,    0x2c,    0x2d,    0x2e,    0x2f,
/*0x30*/    0x30,    0x31,    0x32,    0x33,    0x34,    0x35,    0x36,    0x37,
/*0x38*/    0x38,    0x39,    0x3a,    0x3b,    0x3c,    0x3d,    0x3e,    0x3f,
/*0x40*/    0xa1,    0x41,    0x42,    0x43,    0x44,    0x45,    0x46,    0x47,
/*0x48*/    0x48,    0x49,    0x4a,    0x4b,    0x4c,    0x4d,    0x4e,    0x4f,
/*0x50*/    0x50,    0x51,    0x52,    0x53,    0x54,    0x55,    0x56,    0x57,
/*0x58*/    0x58,    0x59,    0x5a,    0xc4,    0xd6,    0xd1,    0xdc,    0xa7,
/*0x60*/    0xbf,    0x61,    0x62,    0x63,    0x64,    0x65,    0x66,    0x67,
/*0x68*/    0x68,    0x69,    0x6a,    0x6b,    0x6c,    0x6d,    0x6e,    0x6f,
/*0x70*/    0x70,    0x71,    0x72,    0x73,    0x74,    0x75,    0x76,    0x77,
/*0x78*/    0x78,    0x79,    0x7a,    0xe4,    0xf6,    0xf1,    0xfc,    0xe0,
};


static const _BYTE GsmToIso8859_15[GSM_CHARACTER_SET_SIZE] =
{         /*   +0x0     +0x1     +0x2     +0x3    +0x4     +0x5    +0x6    +0x7  */
/*0x00*/    0x40,    0xa3,    0x24,    0xa5,    0xe8,    0xe9,    0xf9,    0xec,
/*0x08*/    0xf2,    0xc7,    0x0a,    0xd8,    0xf8,    0x0d,    0xc5,    0xe5,
/*0x10*/       0,    0x5f,       0,       0,       0,       0,       0,       0,
/*0x18*/       0,       0,       0,    0x20,    0xc6,    0xe6,    0xdf,    0xc9,
/*0x20*/    0x20,    0x21,    0x22,    0x23,       0,    0x25,    0x26,    0x27,
/*0x28*/    0x28,    0x29,    0x2a,    0x2b,    0x2c,    0x2d,    0x2e,    0x2f,
/*0x30*/    0x30,    0x31,    0x32,    0x33,    0x34,    0x35,    0x36,    0x37,
/*0x38*/    0x38,    0x39,    0x3a,    0x3b,    0x3c,    0x3d,    0x3e,    0x3f,
/*0x40*/    0xa1,    0x41,    0x42,    0x43,    0x44,    0x45,    0x46,    0x47,
/*0x48*/    0x48,    0x49,    0x4a,    0x4b,    0x4c,    0x4d,    0x4e,    0x4f,
/*0x50*/    0x50,    0x51,    0x52,    0x53,    0x54,    0x55,    0x56,    0x57,
/*0x58*/    0x58,    0x59,    0x5a,    0xc4,    0xd6,    0xd1,    0xdc,    0xa7,
/*0x60*/    0xbf,    0x61,    0x62,    0x63,    0x64,    0x65,    0x66,    0x67,
/*0x68*/    0x68,    0x69,    0x6a,    0x6b,    0x6c,    0x6d,    0x6e,    0x6f,
/*0x70*/    0x70,    0x71,    0x72,    0x73,    0x74,    0x75,    0x76,    0x77,
/*0x78*/    0x78,    0x79,    0x7a,    0xe4,    0xf6,    0xf1,    0xfc,    0xe0,
};


///////////////////////


static const _BYTE ascii_to_extension_gsm1[] = 
{
 // 0x5b, 0x5c, 0x5d, 0x5e
	0x3c, 0x2f, 0x3e, 0x14
};

static const _BYTE ascii_to_extension_gsm2[] = 
{
 // 0x7b, 0x7c, 0x7d, 0x7e
	0x28, 0x40, 0x29, 0x3d
};

static const _BYTE iso_8859_15_to_gsm[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] =
{
/*0x00*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x08*/  NON_GSM,  NON_GSM,     0x0a,  NON_GSM,  NON_GSM,     0x0d,  NON_GSM,  NON_GSM, 
/*0x10*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x18*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x20*/     0x20,     0x21,     0x22,     0x23,     0x02,     0x25,     0x26,     0x27, 
/*0x28*/     0x28,     0x29,     0x2a,     0x2b,     0x2c,     0x2d,     0x2e,     0x2f, 
/*0x30*/     0x30,     0x31,     0x32,     0x33,     0x34,     0x35,     0x36,     0x37, 
/*0x38*/     0x38,     0x39,     0x3a,     0x3b,     0x3c,     0x3d,     0x3e,     0x3f, 
/*0x40*/     0x00,     0x41,     0x42,     0x43,     0x44,     0x45,     0x46,     0x47, 
/*0x48*/     0x48,     0x49,     0x4a,     0x4b,     0x4c,     0x4d,     0x4e,     0x4f, 
/*0x50*/     0x50,     0x51,     0x52,     0x53,     0x54,     0x55,     0x56,     0x57, 
/*0x58*/     0x58,     0x59,     0x5a,     0x1b,     0x1b,     0x1b,     0x1b,     0x11, 
/*0x60*/  NON_GSM,     0x61,     0x62,     0x63,     0x64,     0x65,     0x66,     0x67, 
/*0x68*/     0x68,     0x69,     0x6a,     0x6b,     0x6c,     0x6d,     0x6e,     0x6f, 
/*0x70*/     0x70,     0x71,     0x72,     0x73,     0x74,     0x75,     0x76,     0x77, 
/*0x78*/     0x78,     0x79,     0x7a,     0x1b,     0x1b,     0x1b,     0x1b,  NON_GSM, 
/*0x80*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x88*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x90*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x98*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xa0*/  NON_GSM,     0x40,  NON_GSM,     0x01,     0x1b,     0x03,  NON_GSM,     0x5f, 
/*0xa8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xb0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xb8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x60, 
/*0xc0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x5b,     0x0e,     0x1c,     0x09, 
/*0xc8*/  NON_GSM,     0x1f,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xd0*/  NON_GSM,     0x5d,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x5c,  NON_GSM, 
/*0xd8*/     0x0b,  NON_GSM,  NON_GSM,  NON_GSM,     0x5e,  NON_GSM,  NON_GSM,     0x1e, 
/*0xe0*/     0x7f,  NON_GSM,  NON_GSM,  NON_GSM,     0x7b,     0x0f,     0x1d,  NON_GSM, 
/*0xe8*/     0x04,     0x05,  NON_GSM,  NON_GSM,     0x07,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xf0*/  NON_GSM,     0x7d,     0x08,  NON_GSM,  NON_GSM,  NON_GSM,     0x7c,  NON_GSM, 
/*0xf8*/     0x0c,     0x06,  NON_GSM,  NON_GSM,     0x7e,  NON_GSM,  NON_GSM,  NON_GSM
};

static const _BYTE cp858_to_gsm[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] =
{
/*0x00*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x08*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x10*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x5f,  NON_GSM,  NON_GSM, 
/*0x18*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x20*/     0x20,     0x21,     0x22,     0x23,     0x02,     0x25,     0x26,     0x27, 
/*0x28*/     0x28,     0x29,     0x2a,     0x2b,     0x2c,     0x2d,     0x2e,     0x2f, 
/*0x30*/     0x30,     0x31,     0x32,     0x33,     0x34,     0x35,     0x36,     0x37, 
/*0x38*/     0x38,     0x39,     0x3a,     0x3b,     0x3c,     0x3d,     0x3e,     0x3f, 
/*0x40*/     0x00,     0x41,     0x42,     0x43,     0x44,     0x45,     0x46,     0x47, 
/*0x48*/     0x48,     0x49,     0x4a,     0x4b,     0x4c,     0x4d,     0x4e,     0x4f, 
/*0x50*/     0x50,     0x51,     0x52,     0x53,     0x54,     0x55,     0x56,     0x57, 
/*0x58*/     0x58,     0x59,     0x5a,     0x1b,     0x1b,     0x1b,     0x1b,     0x11, 
/*0x60*/  NON_GSM,     0x61,     0x62,     0x63,     0x64,     0x65,     0x66,     0x67, 
/*0x68*/     0x68,     0x69,     0x6a,     0x6b,     0x6c,     0x6d,     0x6e,     0x6f, 
/*0x70*/     0x70,     0x71,     0x72,     0x73,     0x74,     0x75,     0x76,     0x77, 
/*0x78*/     0x78,     0x79,     0x7a,     0x1b,     0x1b,     0x1b,     0x1b,  NON_GSM, 
/*0x80*/     0x09,     0x7e,     0x05,  NON_GSM,     0x7b,     0x7f,     0x0f,  NON_GSM, 
/*0x88*/  NON_GSM,  NON_GSM,     0x04,  NON_GSM,  NON_GSM,     0x07,     0x5b,     0x0e, 
/*0x90*/     0x1f,     0x1d,     0x1c,  NON_GSM,     0x7c,     0x08,  NON_GSM,     0x06, 
/*0x98*/  NON_GSM,     0x5c,     0x5e,     0x0c,     0x01,     0x0b,  NON_GSM,  NON_GSM, 
/*0xa0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x7d,     0x5d,  NON_GSM,  NON_GSM, 
/*0xa8*/     0x60,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x40,  NON_GSM,  NON_GSM, 
/*0xb0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xb8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x03,  NON_GSM, 
/*0xc0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xc8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x24, 
/*0xd0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x1b,  NON_GSM,  NON_GSM, 
/*0xd8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x03,  NON_GSM,  NON_GSM, 
/*0xe0*/  NON_GSM,     0x1e,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xe8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xf0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x5f,  NON_GSM,  NON_GSM, 
/*0xf8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM 
};

static const _BYTE cp1252_to_gsm[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] =
{
/*0x00*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x08*/  NON_GSM,  NON_GSM,     0x0a,  NON_GSM,  NON_GSM,     0x0d,  NON_GSM,  NON_GSM, 
/*0x10*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x18*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x20*/     0x20,     0x21,     0x22,     0x23,     0x02,     0x25,     0x26,     0x27, 
/*0x28*/     0x28,     0x29,     0x2a,     0x2b,     0x2c,     0x2d,     0x2e,     0x2f, 
/*0x30*/     0x30,     0x31,     0x32,     0x33,     0x34,     0x35,     0x36,     0x37, 
/*0x38*/     0x38,     0x39,     0x3a,     0x3b,     0x3c,     0x3d,     0x3e,     0x3f, 
/*0x40*/     0x00,     0x41,     0x42,     0x43,     0x44,     0x45,     0x46,     0x47, 
/*0x48*/     0x48,     0x49,     0x4a,     0x4b,     0x4c,     0x4d,     0x4e,     0x4f, 
/*0x50*/     0x50,     0x51,     0x52,     0x53,     0x54,     0x55,     0x56,     0x57, 
/*0x58*/     0x58,     0x59,     0x5a,     0x1b,     0x1b,     0x1b,     0x1b,     0x11, 
/*0x60*/  NON_GSM,     0x61,     0x62,     0x63,     0x64,     0x65,     0x66,     0x67, 
/*0x68*/     0x68,     0x69,     0x6a,     0x6b,     0x6c,     0x6d,     0x6e,     0x6f, 
/*0x70*/     0x70,     0x71,     0x72,     0x73,     0x74,     0x75,     0x76,     0x77, 
/*0x78*/     0x78,     0x79,     0x7a,     0x1b,     0x1b,     0x1b,     0x1b,  NON_GSM, 
/*0x80*/     0x1b,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x88*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x90*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x98*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xa0*/  NON_GSM,     0x40,  NON_GSM,     0x01,     0x24,     0x03,  NON_GSM,     0x5f, 
/*0xa8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xb0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xb8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x60, 
/*0xc0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x5b,     0x0e,     0x1c,     0x09, 
/*0xc8*/  NON_GSM,     0x1f,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xd0*/  NON_GSM,     0x5d,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x5c,  NON_GSM, 
/*0xd8*/     0x0b,  NON_GSM,  NON_GSM,  NON_GSM,     0x5e,  NON_GSM,  NON_GSM,     0x1e, 
/*0xe0*/     0x7f,  NON_GSM,  NON_GSM,  NON_GSM,     0x7b,     0x0f,     0x1d,  NON_GSM, 
/*0xe8*/     0x04,     0x05,  NON_GSM,  NON_GSM,     0x07,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xf0*/  NON_GSM,     0x7d,     0x08,  NON_GSM,  NON_GSM,  NON_GSM,     0x7c,  NON_GSM, 
/*0xf8*/     0x0c,     0x06,  NON_GSM,  NON_GSM,     0x7e,  NON_GSM,  NON_GSM,  NON_GSM 
};

static const _BYTE cp437_to_gsm[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] =
{
/*0x00*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x08*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x10*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x5f,  NON_GSM,  NON_GSM, 
/*0x18*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0x20*/     0x20,     0x21,     0x22,     0x23,     0x02,     0x25,     0x26,     0x27, 
/*0x28*/     0x28,     0x29,     0x2a,     0x2b,     0x2c,     0x2d,     0x2e,     0x2f, 
/*0x30*/     0x30,     0x31,     0x32,     0x33,     0x34,     0x35,     0x36,     0x37, 
/*0x38*/     0x38,     0x39,     0x3a,     0x3b,     0x3c,     0x3d,     0x3e,     0x3f, 
/*0x40*/     0x00,     0x41,     0x42,     0x43,     0x44,     0x45,     0x46,     0x47, 
/*0x48*/     0x48,     0x49,     0x4a,     0x4b,     0x4c,     0x4d,     0x4e,     0x4f, 
/*0x50*/     0x50,     0x51,     0x52,     0x53,     0x54,     0x55,     0x56,     0x57, 
/*0x58*/     0x58,     0x59,     0x5a,     0x1b,     0x1b,     0x1b,     0x1b,     0x11, 
/*0x60*/  NON_GSM,     0x61,     0x62,     0x63,     0x64,     0x65,     0x66,     0x67, 
/*0x68*/     0x68,     0x69,     0x6a,     0x6b,     0x6c,     0x6d,     0x6e,     0x6f, 
/*0x70*/     0x70,     0x71,     0x72,     0x73,     0x74,     0x75,     0x76,     0x77, 
/*0x78*/     0x78,     0x79,     0x7a,     0x1b,     0x1b,     0x1b,     0x1b,  NON_GSM, 
/*0x80*/     0x09,     0x7e,     0x05,  NON_GSM,     0x7b,     0x7f,     0x0f,  NON_GSM, 
/*0x88*/  NON_GSM,  NON_GSM,     0x04,  NON_GSM,  NON_GSM,     0x07,     0x5b,     0x0e, 
/*0x90*/     0x1f,     0x0f,     0x1c,  NON_GSM,     0x7c,     0x08,  NON_GSM,     0x06, 
/*0x98*/  NON_GSM,     0x5c,     0x5e,  NON_GSM,     0x01,     0x03,     0x5f,  NON_GSM, 
/*0xa0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x7d,     0x5d,  NON_GSM,  NON_GSM, 
/*0xa8*/     0x60,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,     0x40,  NON_GSM,  NON_GSM, 
/*0xb0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xb8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xc0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xc8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xd0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xd8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xe0*/  NON_GSM,  NON_GSM,     0x13,  NON_GSM,     0x18,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xe8*/     0x12,     0x19,     0x15,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xf0*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM, 
/*0xf8*/  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM,  NON_GSM
};


#if 0
/* Conversion table for ASCII to SMS Default */
static const _BYTE ascii_to_gsmdefault_table[] =
{       /* +0x0     +0x1     +0x2     +0x3     +0x4     +0x5     +0x6     +0x7 */
/*0x00*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0x08*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0x10*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0x18*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',
/*0x20*/   ' ',     '!',    0x22,     '#',     '$',     '%',     '&',    0x27, 
/*0x28*/   '(',     ')',     '*',     '+',     ',',     '-',     '.',     '/', 
/*0x30*/   '0',     '1',     '2',     '3',     '4',     '5',     '6',     '7',     
/*0x38*/   '8',     '9',     ':',     ';',     '<',     '=',     '>',     '?', 
/*0x40*/  0x00,     'A',     'B',     'C',     'D',     'E',     'F',     'G',     
/*0x48*/   'H',     'I',     'J',     'K',     'L',     'M',     'N',     'O',
/*0x50*/   'P',     'Q',     'R',     'S',     'T',     'U',     'V',     'W',     
/*0x58*/   'X',     'Y',     'Z',     '[',     '\\',     ']',    '^',     '_',
/*0x60*/   ' ',     'a',     'b',     'c',     'd',     'e',     'f',     'g',     
/*0x68*/   'h',     'i',     'j',     'k',     'l',     'm',     'n',     'o',
/*0x70*/   'p',     'q',     'r',     's',     't',     'u',     'v',     'w',     
/*0x78*/   'x',     'y',     'z',     '{',     '|',     '}',     '~',     ' ',
/*0x80*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0x88*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0x90*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0x98*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',
/*0xA0*/   ' ',    0x40,     ' ',    0x01,    0x24,    0x03,     ' ',    0x5F, 
/*0xA8*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',
/*0xB0*/   ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0xB8*/  0x0B,     ' ',     ' ',     ' ',     ' ',     ' ',     ' ',    0x60,
/*0xC0*/   ' ',     ' ',     ' ',     ' ',    0x5B,    0x0E,    0x1C,    0x09, 
/*0xC8*/   ' ',    0x1F,     ' ',     ' ',     ' ',     ' ',     ' ',     ' ', 
/*0xD0*/   ' ',    0x5D,     ' ',     ' ',     ' ',     ' ',    0x5C,     ' ', 
/*0xD8*/   ' ',     ' ',     ' ',     ' ',    0x5E,     ' ',     ' ',    0x1E, 
/*0xE0*/  0x7F,    ' ',      ' ',     ' ',    0x7B,    0x0F,    0x1D,     ' ', 
/*0xE8*/  0x04,    0x05,     ' ',     ' ',    0x07,     ' ',     ' ',     ' ', 
/*0xF0*/   ' ',    0x7D,    0x08,     ' ',     ' ',     ' ',    0x7C,     ' ', 
/*0xF8*/  0x0C,    0x06,     ' ',     ' ',    0x7E,     ' ',     ' ',     ' '
};
#endif


// 0x20ac, 0x65
#define EURO_AT_EXTENSION_GSMDEFAULT_POS	0x65

#define EURO_AT_CP1252_POS					0x80
#define EURO_AT_CP858_POS					0xd5
#define EURO_AT_ISO_8859_15_POS				0xa5
#define EURO_AT_CP437_POS					0	// no euro

#if defined USE_ISO_8859_15_TABLE
	#define EURO_AT_TABLE_POS	EURO_AT_ISO_8859_15_POS	
#elif defined USE_CP858_TABLE
	#define EURO_AT_TABLE_POS	EURO_AT_CP858_POS	
#elif defined USE_CP437_TABLE
	#define EURO_AT_TABLE_POS	EURO_AT_CP437_POS	
#else
	#define EURO_AT_TABLE_POS	EURO_AT_CP1252_POS	
#endif


GsmCharType mapAsciiToEGsm(_BYTE ascii, _BYTE *gsm1_p, _BYTE *gsm2_p)
{
	GsmCharType		retVal = NON_GSM7BIT;

	if(gsm1_p != NULL && gsm2_p != NULL)
	{
		*gsm1_p = ascii;
		*gsm2_p = NON_GSM;

		if(ascii > GSM_CHARACTER_SET_SIZE)
			return retVal;

		// map from 0x5b~0x5e
		if( ascii>=0x5b && ascii <=0x5e )
		{
			retVal = EXTENDED_GSM7BIT;

			*gsm1_p = EXTENDED_ALPHABET_TABLE;
			*gsm2_p = ascii_to_extension_gsm1[ascii-0x5b];
		}
		// map from 0x7b~0x7e
		else if( ascii>=0x7b && ascii <=0x7e )
		{
			retVal = EXTENDED_GSM7BIT;

			*gsm1_p = EXTENDED_ALPHABET_TABLE;
			*gsm2_p = ascii_to_extension_gsm2[ascii-0x7b];
		}
	}

	return retVal;
}

_BYTE mapCodePageToEGsm(_BYTE ascii)
{
	_BYTE retVal = 0;

// iso-8859-15
#if defined USE_ISO_8859_15_TABLE
	retVal = iso_8859_15_to_gsm[ascii];
// cp858
#elif defined USE_CP858_TABLE
	retVal = cp858_to_gsm[ascii];
// cp437
#elif defined USE_CP437_TABLE
	retVal = cp437_to_gsm[ascii];
// cp1252
#else
	retVal = cp1252_to_gsm[ascii];
#endif

	return retVal;
}

GsmCharType ConvAsciiToEGsm(const _BYTE *asciiStr_p, _WORD asciiLength, _BYTE *gsmStr_p, _WORD *gsmLength_p)
{
	GsmCharType retVal	 = GSM7BIT;
	GsmCharType charType = NON_GSM7BIT;
	_WORD currentGsm 	 = 0;
	_WORD currentAscii   = 0;
	_BYTE	gsmChar1;
	_BYTE 	gsmChar2;
	
	/* Initialise gsm length */
	*gsmLength_p = 0;

	/* Check for valid gsmStr_p */
	if(gsmStr_p != NULL)
	{
		/* Loop through ascii String */
		for(currentAscii = 0; currentAscii < asciiLength; currentAscii++)
		{
			gsmChar1 = mapCodePageToEGsm(asciiStr_p[currentAscii]);

			if( gsmChar1 == EXTENDED_ALPHABET_TABLE )
			{
				// euro
				if(asciiStr_p[currentAscii] == EURO_AT_TABLE_POS)
				{
				    gsmChar1 = EXTENDED_ALPHABET_TABLE;
				    gsmChar2 = EURO_AT_EXTENSION_GSMDEFAULT_POS;
				}
				else
				{
					/* use mapAsciiToEGsm to map ascii to gsm Characters */
					mapAsciiToEGsm(asciiStr_p[currentAscii], &gsmChar1, &gsmChar2);
				}

				/* Set gsmStr to both characters returned in gsmChar1, and gsmChar2 */
				gsmStr_p[currentGsm] = gsmChar1;
				gsmStr_p[currentGsm+1] 	= gsmChar2;

				/* Increment Length by 2 for Extended Character */
				(*gsmLength_p)+=2;
				/* Increment currentGsm by 2 to avoid Extended Characters */
				currentGsm+=2;

				retVal = EXTENDED_GSM7BIT;
			}
			else if( gsmChar1 == NON_GSM )
			{
				/* Store Gsm Character in gsm string */
				gsmStr_p[currentGsm] = SPACE;
				/* Increment Length by 1 */
				(*gsmLength_p)++;
				/* Increment current Gsm by 1 */
				currentGsm++;

				retVal = NON_GSM7BIT;
			}
			else
			{
				/* Store Gsm Character in gsm string */
				gsmStr_p[currentGsm] = gsmChar1;
				/* Increment Length by 1*/
				(*gsmLength_p)++;
				/* Increment current Gsm by 1 */
				currentGsm++;

				retVal = GSM7BIT;
			}
		}
	}
	else
	{
		/* No Valid Gsm Data Buffer return NON_GSM */
		retVal = NON_GSM7BIT;
	}

	return retVal;
}




///////////////////////


static const _BYTE GsmToChar[GSM_CHARACTER_SET_SIZE] =
{         /*+0x0        +0x1        +0x2        +0x3        +0x4        +0x5        +0x6        +0x7*/
/*0x00*/    0x40,       0x9c,       0x24,       0x9d,       0x8a,       0x82,       0x97,       0x8d,
/*0x07*/    0x95,       0x80,       0x0a,       0xed,       0xed,       0x0d,       0x8f,       0x86,
/*0x10*/       0,       0x5f,       0xe8,       0xe2,          0,       0xea,       0xe3,          0,
/*0x18*/    0xe4,       0xe9,          0,       0x20,       0x92,       0x91,       0xe1,       0x90,
/*0x20*/    0x20,       0x21,       0x22,       0x23,       0xAD,       0x25,       0x26,       0x27,
/*0x28*/    0x28,       0x29,       0x2a,       0x2b,       0x2c,       0x2d,       0x2e,       0x2f,
/*0x30*/    0x30,       0x31,       0x32,       0x33,       0x34,       0x35,       0x36,       0x37,
/*0x37*/    0x38,       0x39,       0x3a,       0x3b,       0x3c,       0x3d,       0x3e,       0x3f,
/*0x40*/    0xa1,       0x41,       0x42,       0x43,       0x44,       0x45,       0x46,       0x47,
/*0x48*/    0x48,       0x49,       0x4a,       0x4b,       0x4c,       0x4d,       0x4e,       0x4f,
/*0x50*/    0x50,       0x51,       0x52,       0x53,       0x54,       0x55,       0x56,       0x57,
/*0x58*/    0x58,       0x59,       0x5a,       0x8e,       0x99,       0xa5,       0x9a,       0x20,
/*0x60*/    0xa8,       0x61,       0x62,       0x63,       0x64,       0x65,       0x66,       0x67,
/*0x68*/    0x68,       0x69,       0x6a,       0x6b,       0x6c,       0x6d,       0x6e,       0x6f,
/*0x70*/    0x70,       0x71,       0x72,       0x73,       0x74,       0x75,       0x76,       0x77,
/*0x78*/    0x78,       0x79,       0x7a,       0x84,       0x94,       0xa4,       0x81,       0x85
};


static const _BYTE  Ucs2ToChar[UCS2_TO_GSM_LOOKUP_TABLE_SIZE] =
{         /*  +0x0        +0x1        +0x2        +0x3        +0x4        +0x5        +0x6        +0x7  */
/*0x00*/      0x00,       0x01,       0x02,       0x03,       0x04,       0x05,       0x06,       0x07,
/*0x08*/      0x08,       0x09,       0x0a,       0x0b,       0x0c,       0x0d,       0x0e,       0x0f,
/*0x10*/      0x10,       0x11,       0x12,       0x13,       0x14,       0x15,       0x16,       0x17,
/*0x18*/      0x18,       0x19,       0x1a,       0x1b,       0x1c,       0x1d,       0x1e,       0x1f,
/*0x20*/      0x20,       0x21,       0x22,       0x23,       0xad,       0x25,       0x26,       0x27,
/*0x28*/      0x28,       0x29,       0x2a,       0x2b,       0x2c,       0x2d,       0x2e,       0x2f,
/*0x30*/      0x30,       0x31,       0x32,       0x33,       0x34,       0x35,       0x36,       0x37,
/*0x38*/      0x38,       0x39,       0x3a,       0x3b,       0x3c,       0x3d,       0x3e,       0x3f,
/*0x40*/      0x40,       0x41,       0x42,       0x43,       0x44,       0x45,       0x46,       0x47,
/*0x48*/      0x48,       0x49,       0x4a,       0x4b,       0x4c,       0x4d,       0x4e,       0x4f,
/*0x50*/      0x50,       0x51,       0x52,       0x53,       0x54,       0x55,       0x56,       0x57,
/*0x58*/      0x58,       0x59,       0x5a,       0x5b,       0x5c,       0x5d,       0x5e,       0x5f,
/*0x60*/      0x60,       0x61,       0x62,       0x63,       0x64,       0x65,       0x66,       0x67,
/*0x68*/      0x68,       0x69,       0x6a,       0x6b,       0x6c,       0x6d,       0x6e,       0x6f,
/*0x70*/      0x70,       0x71,       0x72,       0x73,       0x74,       0x75,       0x76,       0x77,
/*0x78*/      0x78,       0x79,       0x7a,       0x7b,       0x7c,       0x7d,       0x7e,       0x7f,
/*0x80*/   NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x88*/   NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x90*/   NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0x98*/   NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,
/*0xa0*/   NON_GSM,       0xad,    NON_GSM,       0x9c,       0xad,       0x9d,    NON_GSM,       0x20,
/*0xa8*/   NON_GSM,    NON_GSM,       0xa6,       0xae,       0xaa,    NON_GSM,    NON_GSM,    NON_GSM,
/*0xb0*/      0xa7,       0xf1,    NON_GSM,    NON_GSM,    NON_GSM,       0xe6,    NON_GSM,       0xfa,
/*0xb8*/   NON_GSM,    NON_GSM,    NON_GSM,       0xaf,       0xab,       0xac,    NON_GSM,       0xa8,
/*0xc0*/   NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0x8e,       0x8f,       0x92,       0x80,
/*0xc8*/   NON_GSM,       0x90,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0xa8,
/*0xd0*/   NON_GSM,       0xa5,    NON_GSM,    NON_GSM,    NON_GSM,    NON_GSM,       0x99,    NON_GSM,
/*0xd8*/      0xed,    NON_GSM,    NON_GSM,    NON_GSM,       0x9a,    NON_GSM,    NON_GSM,       0xe1,
/*0xe0*/      0x85,    NON_GSM,       0x83,    NON_GSM,       0x84,       0x86,       0x91,       0x87,
/*0xe8*/      0x8a,       0x82,       0x88,       0x89,       0x8d,       0xa1,       0x8c,       0x8b,
/*0xf0*/   NON_GSM,       0xa4,       0x95,       0xa2,       0x93,    NON_GSM,       0x94,       0xf6,
/*0xf8*/      0xed,       0x97,       0xa3,       0x96,       0x81,    NON_GSM,    NON_GSM,       0x98
};


/*
 mapGsmToChar
 This function provides a protected lookup for converting a GSM 7bit alphabet character to char characters.
 */
_BYTE mapGsmToChar ( _BYTE gsmChar )
{
	_BYTE retVal = 0;

//  _ASSERT (gsmChar < GSM_CHARACTER_SET_SIZE);
	retVal = GsmToChar[gsmChar];

 	return retVal;
}

/* 
	Converts an array of bytes, each of which should be a valid GSM character (range 0 to 0x7F) to a string of char.
	Stops when a conversion fails and returns the number of successful conversions performed.
*/
_WORD ConvGsm7BitToChar ( _BYTE *charString, const _BYTE *gsmData, _WORD length)
{
	_WORD done = 0;

	while( (done < length) && (gsmData[done] < GSM_CHARACTER_SET_SIZE) )
	{
		charString[done] = mapGsmToChar(gsmData[done]);

		++done;
	}
	
	return done;
}



/*
	mapUcs2ToChar
	This function will try to map a Ucs2 character into the Char alphabet. The mapping may fail.
	The result from this function should always be verified.
 */
_BYTE mapUcs2ToChar ( _WORD ucs2Char )
{
	_BYTE  retVal = NON_GSM;

	if(ucs2Char < UCS2_TO_GSM_LOOKUP_TABLE_SIZE)
	{
		retVal = Ucs2ToChar[ucs2Char];
	}

	return retVal;
}


/*
	Converts a Ucs2 string to Char string.
	Stops when a conversion fails and returns the number of successful conversions performed.
	If the destination gsmData equals PNULL, don't store the conversion result.
 */
_WORD ConvUcs2ToChar ( _BYTE *charStr, const _WORD *ucs2String, _WORD length)
{
	_WORD done = 0;
	
	if( charStr != NULL)
	{
		while(done < length)
		{
			charStr[done] = mapUcs2ToChar(ucs2String[done]);
		
			if(charStr[done] == NON_GSM)
				break;
		
			++done;
		}
	}
	else
	{
		/* If the target gsmData is NULL, just count
		* the characters that can be converted into the GSM character set
		*/
		while(done < length)
		{
			if( mapUcs2ToChar(ucs2String[done]) == NON_GSM)
				break;
			
			++done;
		}
	}
	
	return done;
}

/////////////////////



/*
 * This function determines what the resultant length of an AlphaIdToUcs2 conversion would be, 
 * without performing the conversion.
 * It returns the string length (number of characters, not number of bytes).
 */
_WORD LengthAlphaIdToUcs2( const _BYTE *alphaId, _WORD alphaIdLen )
{
  	_WORD  lengthUcs2;

  	if ( *alphaId < GSM_CHARACTER_SET_SIZE )
  	{
    	lengthUcs2 = alphaIdLen;
  	}
  	else if ( *alphaId == DOUBLE_OCTET_UCS2 )
  	{
    	lengthUcs2 = (alphaIdLen - 1) / 2;
  	}
  	else if ( (*alphaId == SINGLE_OCTET_BASE_POINTER_COMBINED) ||
          	  (*alphaId == DOUBLE_OCTET_BASE_POINTER_COMBINED) )
  	{
    	lengthUcs2 = *(++alphaId);
  	}
  	else
  	{
    	// Invalid AlphaId
    	lengthUcs2 = alphaIdLen;
  	}
  
  	return lengthUcs2;
}


/*
 * Utility function to convert a GSM AlphaId string to a UCS2 string. A maximum length for the resultant UCS2 is required.
 * The function returns the actual length of the string produced.
 */
_WORD ConvAlphaIdToUcs2( _WORD *strDst_p, _WORD maxDstLen, const _BYTE *alphaId, _WORD alphaIdLen )
{
	GsmCharType charType;
	_WORD		Ucs2Char;
	_WORD		alphaIndex = 0;
	_WORD		dstLen = 0;
	const _BYTE *endOfalphaId = alphaId + alphaIdLen;
	const _BYTE maxAlphaIdIndex = alphaIdLen - 1;

	if ( *alphaId < GSM_CHARACTER_SET_SIZE)
	{
		/* Byte-aligned GSM 7-bit */
		while ( (maxDstLen-- != 0) && (alphaIndex < alphaIdLen) )
		{
			if (alphaIndex == maxAlphaIdIndex)
			{
				charType = mapEGsmToUcs2(alphaId[alphaIndex], NON_GSM, strDst_p);
			}
			else
			{
				charType = mapEGsmToUcs2(alphaId[alphaIndex], alphaId[alphaIndex+1], strDst_p);
			}

			switch(charType)
			{
			case NON_GSM7BIT :
				maxDstLen = 0;
				break;

			case GSM7BIT:
				alphaIndex++;
				dstLen++;
				break;

			case EXTENDED_GSM7BIT :
				alphaIndex+=2;
				dstLen++;
				break;
			}

			/* Increment Destination Pointer To Next Character */
			strDst_p++;
		}
	}
	else if ( *alphaId == DOUBLE_OCTET_UCS2 )	// 0x80
	{
		/* Fully expanded UCS2 */
		alphaId++;

		while ( (maxDstLen-- != 0) && (alphaId < endOfalphaId) )
		{
			Ucs2Char = (_WORD)(*alphaId++ << 8);
			Ucs2Char |= *alphaId++;

			if (Ucs2Char == 0xFFFF || Ucs2Char == 0)
			{
				*strDst_p = 0;
				maxDstLen = 0;
			}
			else
			{
				*strDst_p++ = Ucs2Char;
				dstLen++;
			}
		}
	}
	else if ( *alphaId == SINGLE_OCTET_BASE_POINTER_COMBINED )	// 0x81
	{
		/* Single Octet base pointer */
		_WORD basePointer;

		/* Length of string (limited to max destination string length) */
		alphaId++;
		maxDstLen = (*alphaId > maxDstLen) ? maxDstLen : *alphaId;

		/* Ucs2 half-page offset ( binary: 0xxx xxxx x000 0000 ) */
		alphaId++;
		basePointer = (_WORD)(*alphaId << 7);

		alphaId++;

		while ( (maxDstLen-- != 0) && (alphaIndex < alphaIdLen))
		{
			if(alphaId[alphaIndex] < GSM_CHARACTER_SET_SIZE)
			{
				charType = mapEGsmToUcs2(alphaId[alphaIndex], alphaId[alphaIndex+1], strDst_p);

				switch(charType)
				{
				case NON_GSM7BIT :
					*strDst_p++ = (_WORD)(*alphaId++ & 0x7F) | basePointer;
					++dstLen;
					break;

				case GSM7BIT:
					alphaIndex++;
					dstLen++;
					break;

				case EXTENDED_GSM7BIT :
					alphaIndex+=2;
					dstLen++;
					break;
				}
			}
			else
			{
				*strDst_p = (_WORD)(alphaId[alphaIndex++] & 0x7F) | basePointer;
				++dstLen;
			}

			/* Increment Destination Pointer To Next Character */
			strDst_p++;
		}
	}
	else if ( *alphaId == DOUBLE_OCTET_BASE_POINTER_COMBINED )	// 0x82
	{
		/* Double octet base pointer */
		_WORD basePointer;

		/* Length of string (limited to max destination string length) */
		alphaId++;
		maxDstLen = (*alphaId > maxDstLen) ? maxDstLen : *alphaId;

		/* Ucs2 half-page offset ( Binary: aaaa aaaa bbbb bbbb ) */
		alphaId++;
		basePointer = (_WORD)(*alphaId++ << 8);
		basePointer |= *alphaId;

		alphaId++;
		while ( maxDstLen-- != 0 && alphaIdLen-- != 0)
		{
			if(alphaId[alphaIndex] < GSM_CHARACTER_SET_SIZE)
			{
				charType = mapEGsmToUcs2(alphaId[alphaIndex], alphaId[alphaIndex+1], strDst_p);

				switch(charType)
				{
				case NON_GSM7BIT :
					*strDst_p = (_WORD)(*alphaId++ &0x7F) + basePointer;
					++dstLen;
					break;

				case GSM7BIT:
					alphaIndex++;
					dstLen++;
					break;

				case EXTENDED_GSM7BIT :
					alphaIndex+=2;
					dstLen++;
					break;
				}
			}
			else
			{
				*strDst_p = (_WORD)(alphaId[alphaIndex++] & 0x7F) + basePointer;
				++dstLen;
			}

			/* Increment Destination Pointer To Next Character */
			strDst_p++;
		}
	}
	else
	{
		*strDst_p = 0;
	}

	return dstLen;
}


/*
   The Alpha Identifier format is specified in the Etsi SIM-ME Interface specification GSM 11.11 (Annex B).
   The function first identifies the tightest coding format that is achievable of the four legal schemes specified in GSM11.11
   It then performs the conversion.
   It returns the actual AlphaId length
 */
_WORD ConvUcs2ToAlphaId ( _BYTE *alphaId_p, _WORD maxAlphaIdLen, const _WORD *strSrc_p, _WORD srcLen )
{
	GsmCharType charType = NON_GSM7BIT;
	_WORD       lowestUcs2Char = 0;
	_WORD       highestUcs2Char = 0;
	_WORD       actualAlphaIdLength = 0;
	const _WORD *startOfUcs2String_p = strSrc_p;
	const _WORD *endOfUcs2String_p = strSrc_p + srcLen;
	_WORD       basePointer = 0;
	_BYTE       *numberOfChars = alphaId_p + 1;
	_WORD       strIndex;
	_BYTE       gsmChar1;
	_BYTE       gsmChar2;

	_BOOL		allGsm = TRUE;
	_BOOL		countChars = FALSE;

	AlphaIdCodingScheme alphaIdCodingScheme;

	strIndex = 0;

	/* Find out the most compact coding scheme that can be used */
	while ( *strSrc_p != 0 && *strSrc_p != 0xFFFF && endOfUcs2String_p > strSrc_p)
	{
		charType = mapUcs2ToEGsm(*strSrc_p, &gsmChar1, &gsmChar2);

		if(charType == NON_GSM7BIT)
		{
			/* This is the first UCS2 Char found */
			if(allGsm == TRUE)
			{
				highestUcs2Char = *strSrc_p;
				lowestUcs2Char = *strSrc_p;
			}
			/* Make a note that the UCS2 string can't be simply translated to the GSM 7bit.  */
			allGsm = FALSE;

			/* As the AlphaId will require "encoding" rather than just translating, reset the length to zero. This will be incremented in the encoding phase (later).  */
			actualAlphaIdLength = 0;

			/* When chars not in the GSM character set have been detected, it is necessary to check the range */
			if(*strSrc_p > highestUcs2Char) 
				highestUcs2Char = *strSrc_p;
			else if (*strSrc_p < lowestUcs2Char) 
				lowestUcs2Char = *strSrc_p;

			if( (highestUcs2Char - lowestUcs2Char) >= GSM_CHARACTER_SET_SIZE ) 
				break;
		}
		else
		{
			/* May as well store the GSM values in the hope that the entire string is "allGsm" */
			switch(charType)
			{
			case NON_GSM7BIT:
				break;

			case GSM7BIT:
				if(actualAlphaIdLength < maxAlphaIdLen)
				{
					alphaId_p[actualAlphaIdLength++] = gsmChar1;
				}
				break;

			case EXTENDED_GSM7BIT:
				if(actualAlphaIdLength < maxAlphaIdLen-1)
				{
					alphaId_p[actualAlphaIdLength++] = gsmChar1;
					alphaId_p[actualAlphaIdLength++] = gsmChar2;
				}
				else
				{
					if(actualAlphaIdLength < maxAlphaIdLen)
					{
						alphaId_p[actualAlphaIdLength++] = gsmChar1;
					}
				}
				break;
			}
		}

		strSrc_p++;
		strIndex++;
	}

	strSrc_p = startOfUcs2String_p;

	if( !allGsm )
		actualAlphaIdLength = 0;

	/* Back to the start of the Ucs2 string */
	/* Fill out the 'coding header' */
	if( allGsm )
	{
		/* Simple! */
		alphaIdCodingScheme = OCTET_ALIGNED_GSM_7BIT;
	}
	else if((highestUcs2Char - lowestUcs2Char) >= GSM_CHARACTER_SET_SIZE)
	{
		/*
		  AlphaId structure:
		  Octet:    |  1   |  2   |  3   |  4   |  5   | ... |  n   |
		  Contains: | 0x80 | MSB1 | LSB1 | MSB2 | LSB2 |     | 0xFF |
		  */
		_ASSERT(maxAlphaIdLen >= 1);

		alphaIdCodingScheme = DOUBLE_OCTET_UCS2;
		alphaId_p[actualAlphaIdLength++] = (_BYTE) alphaIdCodingScheme;
	}
	else if(  ((highestUcs2Char & 0x7f80) == (lowestUcs2Char & 0x7f80))
				&& ((highestUcs2Char & 0x8000) == 0) )
	{
		/*
		  AlphaId structure:
		  Octet:    |  1   |  2   |  3   |  4   |  5   | ... |
		  Contains: | 0x81 | # Ch | baseP| Ch1  | Ch2  |     |
		*/
		_ASSERT(maxAlphaIdLen >= 3);
		alphaIdCodingScheme = SINGLE_OCTET_BASE_POINTER_COMBINED;
		alphaId_p[actualAlphaIdLength++] = (_BYTE) alphaIdCodingScheme;

		countChars = TRUE;
		alphaId_p[actualAlphaIdLength++] = 0;

		basePointer = lowestUcs2Char & 0x7f80;
		alphaId_p[actualAlphaIdLength++] = basePointer >> 7;
	}
	else
	{
		/*
		  AlphaId structure:
		  Octet:    |  1   |  2   |     3    |     4    |  5   |  6   | ... |
		  Contains: | 0x82 | # Ch | base MSB | base LSB | Ch2  | Ch1  |     |
		*/
		_ASSERT(maxAlphaIdLen >= 4);
		alphaIdCodingScheme = DOUBLE_OCTET_BASE_POINTER_COMBINED;
		alphaId_p[actualAlphaIdLength++] = (_BYTE) alphaIdCodingScheme;

		countChars = TRUE;
		alphaId_p[actualAlphaIdLength++] = 0;

		basePointer = lowestUcs2Char;

		/* Base pointer MSByte */
		alphaId_p[actualAlphaIdLength++] = (_BYTE)(basePointer >> 8);

		/* Base pointer LSByte */
		alphaId_p[actualAlphaIdLength++] = (_BYTE)(basePointer & 0xFF);
	}

	if(!allGsm)
	{
		/* Now we know the best coding scheme (standard GSM will already have been completed), code up the AlphaId  */
		_BOOL encodingComplete = FALSE;

		while( encodingComplete == FALSE)
		{
			if( *strSrc_p == 0 ) 
				encodingComplete = TRUE;
			else if (*strSrc_p == 0xFFFF) 
				encodingComplete = TRUE;
			else if (strSrc_p == endOfUcs2String_p) 
				encodingComplete = TRUE;
			else if (actualAlphaIdLength == maxAlphaIdLen) 
				encodingComplete = TRUE;
			else
			{
				/* Let's ENCODE !!!! */
				if(alphaIdCodingScheme == DOUBLE_OCTET_UCS2)
				{
					if( actualAlphaIdLength + 1 < maxAlphaIdLen )
					{
						alphaId_p[actualAlphaIdLength++] = (_BYTE)( *strSrc_p >> 8 );
						alphaId_p[actualAlphaIdLength++] = (_BYTE)( *strSrc_p & 0xFF );
					}
					else
					{
						/* Can't squeeze a whole Ucs2 char in so exit */
						break;
					}
				}
				else
				{
					if(countChars)
					{
						/* Some of the AlphaId 'coding schemes' contain a number-of-chars byte */
						*numberOfChars = (_BYTE)(*numberOfChars + 1);
					}

					charType = mapUcs2ToEGsm(*strSrc_p, &gsmChar1, &gsmChar2);

					if(charType == EXTENDED_GSM7BIT )
					{
						if(actualAlphaIdLength < maxAlphaIdLen - 1)
						{
							alphaId_p[actualAlphaIdLength++] = gsmChar1;
							alphaId_p[actualAlphaIdLength++] = gsmChar2;
						}
						else
						{
							if(actualAlphaIdLength < maxAlphaIdLen )
							{
								alphaId_p[actualAlphaIdLength++] = gsmChar1;
							}
						}
					}
					else if(charType == GSM7BIT)
					{
						if(actualAlphaIdLength < maxAlphaIdLen )
						{
							alphaId_p[actualAlphaIdLength++] = gsmChar1;
						}
					}
					else
					{
						if(actualAlphaIdLength < maxAlphaIdLen)
						{
							alphaId_p[actualAlphaIdLength++] = (_BYTE)(0x80 | (*strSrc_p - basePointer));
						}
					}

				}
				strSrc_p++;
			}
		}
	}

	/* Pad the remaining octets in the AlphaId with 0xFF's */
	if( (alphaIdCodingScheme == OCTET_ALIGNED_GSM_7BIT) ||
		(alphaIdCodingScheme == DOUBLE_OCTET_UCS2) )
	{
		_BYTE *padding_p = &alphaId_p[actualAlphaIdLength];

		while(padding_p < &alphaId_p[maxAlphaIdLen])
		{
			*padding_p++ = 0xFF;
		}
	}

	return actualAlphaIdLength;
}


//////////////////////////////////////////////


_WORD Encode7BitPackedData (_BYTE *buffer, _BYTE *msgData, _WORD msgLen, _WORD maxChars)
{
    _WORD	i;
    _WORD	bufferIndex = 0;
    _BYTE	numOfShifts = 0;

    /* Check for too much user data */
    if ( msgLen > maxChars )
    {
        /* Input data too big Recovery code: Reduce message length (number of characters) and truncate full message length.   */
        msgLen = maxChars;
    }

    for (i = 0; i<msgLen; i++)
    {
        /* Set the appropriate number of bits in this new byte */
        if (numOfShifts != 7)
        {
            /* If shifts equals seven we don't actually right into the current byte pointed to by bufferIndex */
            buffer[bufferIndex] = (_BYTE) (msgData[i] >> numOfShifts);
        }

        /* Set the appropriate number of bits in the previous byte */
        if (numOfShifts != 0)
        {
            buffer[bufferIndex-1] |= (_BYTE) (msgData[i] << (8-numOfShifts));
        }

        numOfShifts = (_BYTE)((numOfShifts+1) % 8);

        /* Increment buffer index, unless numOfShifts is 0, in which case we have 'saved' a whole byte so do not increment the index. */
        if (numOfShifts != 0)
        {
            bufferIndex++;
        }
    }
	
    /* Number of bytes written */
    return bufferIndex;
}


/*
	NOTE: although this is a utility function there is a special case when the
	final byte is padded with seven zeros in bits b7-b1. This should be ignored
	to avoid spurious '@' charcters being interpreted
 */
 _WORD Decode7BitPackedData (_BYTE *destData, _WORD destLength, _BYTE *sourceData, _WORD sourceLength)
{
    /* Take the encoded data pointed to by [buffer] and decode it into 7 bit bytes according to GSM 03.38. */
    _WORD	decodedLength = 0;
    _WORD	inputIndex = 0;
    _BYTE	numOfShifts = 0;

    while ( (inputIndex < sourceLength) && (decodedLength < destLength) )
    {
        /* Read the appropriate number of bits in this new byte */
        destData[decodedLength] = (_BYTE)((sourceData[inputIndex] << numOfShifts));

        /* Ensure top bit (bit 7) is 0 */
        destData[decodedLength] &= 0x7F;

        /* Read the appropriate number of bits in the previous byte */
        if (numOfShifts != 0)
        {
            destData[decodedLength] |= (_BYTE) (sourceData[inputIndex-1] >> (8-numOfShifts));
        }

        numOfShifts = (_BYTE)((numOfShifts+1) % 8);

        /* Increment buffer index, unless numOfShifts is 0, in which case we have shifted by a whole byte so do not increment the index. */
        if (numOfShifts != 0)
        {
            inputIndex++;
            /* Check if byte decoded was last byte of encoded soure data.
               If it is, and the number of source bytes (sourceLength) is a multpiple of 7 then we need to decode it twice as it contains
               data from two characters. This is ALWAYS the case UNLESS the byte is padded with 7 zeros - so check for this first. */
            if ( (inputIndex == sourceLength) && (sourceLength % 7 == 0 ))
            {
                /* SPECIAL CASE :-  decode ONLY if bits b7-b1 are NOT padded with zeros */
                if ( (sourceData[sourceLength-1] & 0xFE) != 0x00 )
                {
                    destData[decodedLength+1] = (_BYTE)(sourceData[sourceLength-1] >> 1);
                    decodedLength++;
                }
            }
        }

        /* Get ready to write the next octet */
        decodedLength++;
    }

    return decodedLength;
}


/////////////////////////////



static const _BYTE BcdMap[] = { '0', '1', '2', '3', '4', '5', '6', '7', 
								'8', '9', 'A', 'B', 'C', 'D', 'E', 'F' };

_BYTE GetBcd(_BYTE *pBase, _INT pos)
{
	_BYTE	val = 0;

	if (pos % 2 != 0)
		val = pBase[pos/2] & 0xF;
	else
		val = (pBase[pos/2] >> 4) & 0xF;

	return val;
}

_VOID Bin2Str(_BYTE *pBcdData, _BYTE bcdDataLen, _BYTE *pTextStr)
{
	_BYTE	val = 0;
	_INT	pos = 0;
	_INT	len = (bcdDataLen * 2);

	while (pos < len)
	{
		val = GetBcd(pBcdData, pos);
		*pTextStr++ = BcdMap[val];
	    pos++;
	}
	*pTextStr = '\0';
}


_VOID PutBcd (_BYTE *pBase, _INT pos, _BYTE val)
{
	if (pos % 2 != 0)
	{
		pBase[pos/2] = (pBase[pos/2] & 0xF0) | (val & 0x0F);
	}
	else
	{
		pBase[pos/2] = (pBase[pos/2] & 0x0F) | ((val << 4) & 0xF0);
	}
}

_INT Str2Bin(_BYTE *pTextStr, _BYTE *pBinStr)
{
	_INT	out=0;
	_INT	val=0;
	_INT	pos=0;
	_INT	len;

	len = strlen((_CHAR *)pTextStr);
	while (pos < len)
	{
		switch (pTextStr[pos])
		{
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			val = pTextStr[pos] - '0';
			PutBcd(pBinStr, out, (_BYTE)val);
        	out++;
			break;
		case 'a':
		case 'A':
			PutBcd(pBinStr, out, 0xA);
			out++;
			break;
		case 'b':
		case 'B':
			PutBcd(pBinStr, out, 0xB);
			out++;
			break;
		case 'c':
		case 'C':
			PutBcd(pBinStr, out, 0xC);
			out++;
			break;
		case 'd':
		case 'D':
			PutBcd(pBinStr, out, 0xD);
			out++;
			break;
		case 'e':
		case 'E':
			PutBcd(pBinStr, out, 0xE);
			out++;
			break;
		case 'f':
		case 'F':
			PutBcd(pBinStr, out, 0xF);
			out++;
			break;
		default:
			break;
		}
		
		pos++;
	}
	
	if (out % 2 != 0)
	{
		PutBcd (pBinStr, out, 0xF);
		out++;
	}

	return (out/2);
}


/////////////////////////////////

// convert to bigendian mode
_WORD ByteSwapWord(_WORD wVal, _WORD wByteOrder)
{
	_WORD	tmp;

	switch(wByteOrder)
	{
	case LITTLE_ENDIAN:
		tmp = (wVal>>8) | ((wVal&0xFF)<<8);
		break;

	case BIG_ENDIAN:
	default:
		tmp = wVal;
		break;
	}

	return tmp;
}

// convert to bigendian mode
_DWORD ByteSwapDword(_DWORD dwVal, _WORD wByteOrder)
{
	_DWORD	tmp;

	switch(wByteOrder)
	{
	case PDP_ENDIAN:	// 3412
		tmp = ( ((dwVal&0xFF000000) >> 8) | 
				((dwVal&0x00FF0000) << 8) | 
				((dwVal&0x0000FF00) >> 8) | 
				((dwVal&0x000000FF) << 8) );
		break;

	case LITTLE_ENDIAN:	// 1234
		tmp = ( (dwVal>>24) | 
				((dwVal&0x00FF0000) >> 8) | 
				((dwVal&0x0000FF00) << 8) | 
				((dwVal&0x000000FF) << 24) );
		break;

	case BIG_ENDIAN:	// 4321
	default:
		tmp = dwVal;
		break;
	}

	return tmp;
}

/////////////////////

/*
	UNICODE UTF-8 
	00000000 - 0000007F 0xxxxxxx 
	00000080 - 000007FF 110xxxxx 10xxxxxx 
	00000800 - 0000FFFF 1110xxxx 10xxxxxx 10xxxxxx 
	00010000 - 001FFFFF 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx 
	00200000 - 03FFFFFF 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 
	04000000 - 7FFFFFFF 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 
*/

#define BYTE_1_REPRESENT		0x80    	/* if <, will be represented in 1 byte */
#define BYTE_2_REPRESENT		0x800   	/* if <, will be represented in 2 bytes */
#define BYTE_3_REPRESENT		0x10000		/* if <, will be represented in 3 bytes */
#define BYTE_4_REPRESENT		0x200000	/* if <, will be represented in 4 bytes */
#define BYTE_5_REPRESENT		0x4000000	/* if <, will be represented in 5 bytes */
#define BYTE_6_REPRESENT		0x8000000	/* if <, will be represented in 6 bytes */

#define MASKBITS 				0x3F
#define MASKBYTE 				0x80		/* Signature mask for 1st byte of 1 byte transformation */
#define MASK2BYTES              0xC0		/* Signature mask for 1st byte of 2 byte transformation */
#define MASK3BYTES              0xE0		/* Signature mask for 1st byte of 3 byte transformation */
#define MASK4BYTES				0xF0		/* Signature mask for 1st byte of 3 byte transformation */
#define MASK5BYTES 				0xF8		/* Signature mask for 1st byte of 4 byte transformation */
#define MASK6BYTES 				0xFC		/* Signature mask for 1st byte of 5 byte transformation */


_DWORD Unicode2ToUTF8(const _WORD *szUnicode, _DWORD dwUnicodeLen, _BYTE *szUtf8)
{
	_DWORD	i = 0;
	_DWORD	out = 0;

	for(i=0; i<dwUnicodeLen; i++)
	{
		// 0xxxxxxx
		if ( szUnicode[i] < BYTE_1_REPRESENT ) /* 1 byte utf8 representation */
		{        
			szUtf8[out++] = (_BYTE)szUnicode[i];
		}
		// 110xxxxx 10xxxxxx
		else if ( szUnicode[i] < BYTE_2_REPRESENT ) /* 2 byte utf8 representation */
		{        
			szUtf8[out++] = (MASK2BYTES | (szUnicode[i] >> 6));
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] & MASKBITS) );
		}
		// 1110xxxx 10xxxxxx 10xxxxxx
		else if ( szUnicode[i] < BYTE_3_REPRESENT ) /* 3 byte utf8 representation */
		{
			szUtf8[out++] = (MASK3BYTES | (szUnicode[i] >> 12) );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 6)  & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] & MASKBITS) );
		}
	}

	return out; /* This value is in bytes */
}


_DWORD UTF8ToUnicode2(const _BYTE *szUtf8, _DWORD dwUtf8Len, _WORD *szUnicode)
{
	_DWORD	i = 0;
	_DWORD	out = 0;

	while ( i < dwUtf8Len )
	{
		// 1110xxxx 10xxxxxx 10xxxxxx
		if( (szUtf8[i] & MASK3BYTES) == MASK3BYTES )
		{
			szUnicode[out++] = ( ((szUtf8[i] & 0x0F) << 12) | 
							   ((szUtf8[i+1] & MASKBITS) << 6) | 
							    (szUtf8[i+2] & MASKBITS) );
			i+=3;
		}
		// 110xxxxx 10xxxxxx
		else if( (szUtf8[i] & MASK2BYTES) == MASK2BYTES )
		{
			szUnicode[out++] = ( ((szUtf8[i] & 0x1F) << 6) | 
								(szUtf8[i+1] & MASKBITS) );
			i+=2;
		}
		// 0xxxxxxx
		else if( (szUtf8[i] & MASKBYTE) == MASKBYTE )
		{
			szUnicode[out++] = szUtf8[i];
			i+=1;
		}
	}

	return out; /* This value is in bytes */
}


//////////////


_DWORD Unicode4ToUTF8(const _DWORD *szUnicode, _DWORD dwUnicodeLen, _BYTE *szUtf8)
{
	_DWORD	i = 0;
	_DWORD	out = 0;

	for(i=0; i<dwUnicodeLen; i++)
	{
		// 0xxxxxxx
		if ( szUnicode[i] < BYTE_1_REPRESENT ) /* 1 byte utf8 representation */
		{
			szUtf8[out++] = (_BYTE)szUnicode[i];
		}
		// 110xxxxx 10xxxxxx
		else if ( szUnicode[i] < BYTE_2_REPRESENT ) /* 2 byte utf8 representation */
		{
			szUtf8[out++] = (MASK2BYTES | (szUnicode[i] >> 6));
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] & MASKBITS) );
		}
		// 1110xxxx 10xxxxxx 10xxxxxx
		else if ( szUnicode[i] < BYTE_3_REPRESENT ) /* 3 byte utf8 representation */
		{
			szUtf8[out++] = (MASK3BYTES | (szUnicode[i] >> 12) );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 6)  & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] & MASKBITS) );
		}
		// 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
		else if ( szUnicode[i] < BYTE_4_REPRESENT ) /* 4 byte utf8 representation */
		{
			szUtf8[out++] = (MASK4BYTES | (szUnicode[i] >> 18) );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 12) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 6) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] & MASKBITS) );
		}
		// 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
		else if ( szUnicode[i] < BYTE_5_REPRESENT ) /* 5 byte utf8 representation */
		{
			szUtf8[out++] = (MASK5BYTES | (szUnicode[i] >> 24) );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 18) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 12) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 6) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] & MASKBITS) );
		}
		// 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
		else if ( szUnicode[i] < BYTE_6_REPRESENT ) /* 6 byte utf8 representation */
		{
			szUtf8[out++] = (MASK6BYTES | (szUnicode[i] >> 30) );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 18) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 12) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] >> 6) & MASKBITS );
			szUtf8[out++] = (  MASKBYTE | (szUnicode[i] & MASKBITS) );
		}
	}

	return out; /* This value is in bytes */
}


_DWORD UTF8ToUnicode4(const _BYTE *szUtf8, _DWORD dwUtf8Len, _DWORD *szUnicode)
{
	_DWORD	i = 0;
	_DWORD	out = 0;

	while ( i < dwUtf8Len )
	{
		// 1111110x 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
		if((szUtf8[i] & MASK6BYTES) == MASK6BYTES)
		{
			szUnicode[out++] = (   ((szUtf8[i] & 0x01) << 30) | 
								 ((szUtf8[i+1] & MASKBITS) << 24) |
								 ((szUtf8[i+2] & MASKBITS) << 18) | 
							 	 ((szUtf8[i+3] & MASKBITS) << 12) |
								 ((szUtf8[i+4] & MASKBITS) << 6) | 
								 ( szUtf8[i+5] & MASKBITS) );
			i+=6;
		}
		// 111110xx 10xxxxxx 10xxxxxx 10xxxxxx 10xxxxxx
		else if((szUtf8[i] & MASK5BYTES) == MASK5BYTES)
		{
			szUnicode[out++] = (  ((szUtf8[i] & 0x03) << 24) | 
								((szUtf8[i+1] & MASKBITS) << 18) |
								((szUtf8[i+2] & MASKBITS) << 12) | 
								((szUtf8[i+3] & MASKBITS) << 6) | 
								( szUtf8[i+4] & MASKBITS) );
			i+=5;
		}
		// 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
		else if((szUtf8[i] & MASK4BYTES) == MASK4BYTES)
		{
			szUnicode[out++] = ( ((szUtf8[i] & 0x07) << 18) | 
							   ((szUtf8[i+1] & MASKBITS) << 12) | 
							   ((szUtf8[i+2] & MASKBITS) << 6) | 
							   ( szUtf8[i+3] & MASKBITS) );
			i+=4;
		}
		// 1110xxxx 10xxxxxx 10xxxxxx
		else if((szUtf8[i] & MASK3BYTES) == MASK3BYTES)
		{
			szUnicode[out++] = ( ((szUtf8[i] & 0x0F) << 12) | 
							   ((szUtf8[i+1] & MASKBITS) << 6) | 
							   ( szUtf8[i+2] & MASKBITS) );
			i+=3;
		}
		// 110xxxxx 10xxxxxx
		else if((szUtf8[i] & MASK2BYTES) == MASK2BYTES)
		{
			szUnicode[out++] = ( ((szUtf8[i] & 0x1F) << 6) | 
							   ( szUtf8[i+1] & MASKBITS) );
			i+=2;
		}
		// 0xxxxxxx
		else if(szUtf8[i] < MASKBYTE)
		{
			szUnicode[out++] = szUtf8[i];
			i+=1;
		}
	}

	return out;
}


///////////////////////////////


_DWORD H80Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode)
{
	_DWORD	i;
	_DWORD	unicode_len = 0;
	_BOOL	bOdd;
	
	if(szUcs2 == NULL)
		return 0;

	if( dwUcs2Len > 0 )
	{
		i = dwUcs2Len;

		/* return length of decode data */
		if ((i%2) != 0) 
		{
			unicode_len = i/2 + 1;
			bOdd = TRUE;
		}
		else 
		{
			unicode_len = i/2;
			bOdd = FALSE;
		}

		if (szUnicode == NULL) 
			return unicode_len;
		memset( szUnicode, 0x00, (sizeof(_WORD)*(unicode_len+1)) );

		/* for loop to the second last unicode word, big-endian */
		for (i = 0; i < unicode_len-1; i++) 
		{
			szUnicode[i] = (szUcs2[2*i] << 8) | szUcs2[2*i+1];
		}

		/* take care of the last unicode word */
		if (bOdd) 
		{
			szUnicode[i] = (szUcs2[2*i] << 8) | 0x00;
		}
		else 
		{
			szUnicode[i] = (szUcs2[2*i] << 8) | szUcs2[2*i+1];
		}
	}
	else
	{
		/* null terminated */
		szUnicode[0] = 0;
		unicode_len = 0;
	}

	return unicode_len;
}


_DWORD H81Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode)
{
	_DWORD	i;
	_DWORD	unicode_len = 0;
	_WORD	base_value = 0;

	if(szUcs2 == NULL)
		return 0;

	if( dwUcs2Len > 0 )
	{
		unicode_len = szUcs2[0]; /* number of characters */
		if ((unicode_len+2) > dwUcs2Len) /* number char & base char*/
		{
			/* limit the data len */
			unicode_len = dwUcs2Len - 2;
		}

		if (szUnicode == NULL) 
			return unicode_len;
		memset( szUnicode, 0x00, (sizeof(_WORD)*(unicode_len+1)) );

		/* base value, contains an 8 bit number which defines bits 15 to 8 of a 16 bit base pointer, 
			where bit 16 is set to zero, and bits 7 to 1 are also set to zero. */
		/*  binary: 0xxx xxxx x000 0000 */
		base_value = (_WORD)(szUcs2[1] << 7);// & 0x3F80;

		for (i = 0; i < unicode_len; i++) 
		{	/* for loop to the last byte in input array */
			if ((szUcs2[i+2] & 0x80) == 0) 
			{
				/* if bit 8 of the octet is set to zero, the remaining 7 bits of the octet 
					contain a GSM Default Alphabet character, */
				szUnicode[i] = GsmToUcs2Ex[szUcs2[i+2]];
			}
			else 
			{
				szUnicode[i] = (szUcs2[i+2] & 0x7F) + base_value;
			}     
		}
	}
	else
	{
		/* null terminated */
		szUnicode[0] = 0;
		unicode_len = 0;
	}

	return unicode_len;
}


_DWORD H82Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode)
{
	_DWORD	i;
	_DWORD	unicode_len = 0;
	_WORD	base_value = 0;

	if(szUcs2 == NULL)
		return 0;

	if( dwUcs2Len > 0 )
	{
		unicode_len = szUcs2[0]; /* number of characters */
		if ((unicode_len+3) > dwUcs2Len) /* chars & two base chars */
		{
			/* limit the data len */
			unicode_len = dwUcs2Len - 3;
		}

		if (szUnicode == NULL) 
			return unicode_len;
		memset( szUnicode, 0x00, (sizeof(_WORD)*(unicode_len+1)) );

		/* base value */
		base_value = (szUcs2[1] << 8) + szUcs2[2];

		for (i = 0; i < unicode_len; i++) 
		{	/* for loop to the last byte in input array */
			if ((szUcs2[i+3] & 0x80) == 0) 
			{
				/* if bit 8 of the octet is set to zero, the remaining 7 bits of the octet 
					contain a GSM Default Alphabet character, */
				szUnicode[i] = GsmToUcs2Ex[szUcs2[i+3]];
			}
			else 
			{
				szUnicode[i] = (szUcs2[i+3] & 0x7F) + base_value;
			}  
		}
	}
	else
	{
		/* null terminated */
		szUnicode[0] = 0;
		unicode_len = 0;
	}

	return unicode_len;
}


_DWORD Ucs2ToUnicode(const _BYTE *szUcs2, _DWORD dwUcs2Len, _WORD *szUnicode)
{
	_DWORD	dwUnicodeLen = 0;

	if((dwUcs2Len > 0) && (szUcs2 != NULL))
	{
		switch( szUcs2[0] ) 
		{
		case 0x80:
			dwUnicodeLen = H80Ucs2ToUnicode((szUcs2+1), (dwUcs2Len-1), szUnicode);
			break;

		case 0x81:
			dwUnicodeLen = H81Ucs2ToUnicode((szUcs2+1), (dwUcs2Len-1), szUnicode);
			break;

		case 0x82:
			dwUnicodeLen = H82Ucs2ToUnicode((szUcs2+1), (dwUcs2Len-1), szUnicode);
			break;

		default:
			_ASSERT(0);
			break;
		}
	}

	return dwUnicodeLen;
}


//////////////////////

//////////////////////

const _BYTE ucs2[] = {0x80, 0x00, 0x30, 0x00, 0x31, 0x00, 0x32};
#include <stdio.h>
void test(void)
{
	_DWORD	dwLen;
	_WORD	szUnicode[10];
	char    buf[20];

    sprintf(buf, "%s:%c", "pipi", 41);
	dwLen = Ucs2ToUnicode(ucs2, sizeof(ucs2)/sizeof(_BYTE), szUnicode);
}

