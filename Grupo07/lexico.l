%{
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>
#include <math.h>
#include "y.tab.h"

#define LEN_LEXEMA         32
#define LEN_FLOAT          32
#define LEN_ID             15
#define LEN_INT_BIN		   16
#define LEN_INT_HEXA	   4

const int INTMAX = 65535;
const int INTMIN = 0;

float FLT_MAX_32b=3.40282347e+38F;
char error_mensaje[1000];

void yyerror(const char * mensaje);

void valida_integer();
void valida_float();
void valida_string();
void valida_id();
void valida_int_bin();
void valida_int_hexa();

%}

%option noyywrap  
%option yylineno 
%option stack
%option noinput
%option nounput
%option noyy_top_state

%x EN_COMENTARIO


DIGITO		[0-9]
DIGITO_BIN	("0"|"1")
LETRA  		[a-zA-Z]
LETRA_HEXA	[A-F]
DIGITO_HEXA	({LETRA_HEXA}|{DIGITO})
ID     		{LETRA}({LETRA}|{DIGITO}|"_")*
CARACTER 	({LETRA}|{DIGITO}|["=",">","<","!",":",";",".","+","-","*","/","@","%","_"," "])
CONSTANTE_ENTERA	({DIGITO})+
CONSTANTE_STRING	("\"")({CARACTER})*("\"") 
CONSTANTE_REAL		({DIGITO})+"."({DIGITO})+|({DIGITO})*"."({DIGITO})+|({DIGITO})+"."({DIGITO})*
CONSTANTE_ENT_HEXA	"0x"({DIGITO_HEXA})+
CONSTANTE_ENT_BIN	"0b"({DIGITO_BIN})+

%%

"DIM"			{ return DIM ; }
"AS"			{ return AS ; }		
"BEGINP"		{ return BEGINP ; }
"ENDP"			{ return ENDP ; }	
"FLOAT"			{ return FLOAT ; } 
"INTEGER"		{ return INTEGER ; }
"STRING"		{ return STRING ; }
"IF"			{ return IF ; }
"THEN"			{ return THEN ; }
"ELSE"			{ return ELSE ; }
"ENDIF"			{ return ENDIF ; }
"AND"      	 	{ return AND ; }
"OR"        	{ return OR ; }
"NOT"       	{ return NOT ; }
"WHILE"       	{ return WHILE ; }
"ENDWHILE"      { return ENDWHILE ; }
"MAXIMO"	    { return MAXIMO ; }
"PUT" 			{ return PUT ; }
"GET" 			{ return GET ; }


"=="			{ return COMP_IGUAL ; }
"!="			{ return COMP_DISTINTO ; }
"<"				{ return COMP_MENOR ; }
">"				{ return COMP_MAYOR ; }
"<="			{ return COMP_MENORIGUAL ; }
">="			{ return COMP_MAYORIGUAL ; }
":"				{ return ASIG ; }
"["				{ return CORCH_ABRE ; }
"]"				{ return CORCH_CIERRA ; }
"{"				{ return LLAVE_ABRE ; }
"}"				{ return LLAVE_CIERRA ; }
","				{ return COMA ; }
";"				{ return PUNTO_Y_COMA ;}
"+"				{ return OP_SUMA ; }
"-"				{ return OP_RESTA ; }
"*"				{ return OP_MULTIPLICACION ; }
"/"				{ return OP_DIVISION ; }
"("				{ return PARENT_ABRE ; }
")"				{ return PARENT_CIERRA ; }

"\n"  			{  }    		
"\t"			{  }
" "				{  }	
"\r\n" 			{  }

{ID}			{  
					valida_id();
					return ID ;
				}

{CONSTANTE_STRING}		{ valida_string(); return CONSTANTE_STRING; }
{CONSTANTE_ENTERA}		{ valida_integer(); return CONSTANTE_ENTERA; }
{CONSTANTE_REAL} 		{ valida_float(); return CONSTANTE_REAL ; }	
{CONSTANTE_ENT_HEXA}	{ valida_int_hexa(); return CONSTANTE_ENT_HEXA; }
{CONSTANTE_ENT_BIN}		{ valida_int_bin(); return CONSTANTE_ENT_BIN; }


"*-"                   { yy_push_state (EN_COMENTARIO);}
<EN_COMENTARIO>"*-"       { yy_push_state(EN_COMENTARIO);}
<EN_COMENTARIO>"-*"       { yy_pop_state();}
<EN_COMENTARIO>(.|\n)       {}
<EN_COMENTARIO><<EOF>>    { printf ("Error, comentario sin cerrar\n");
                         exit (1);}


.				{
				  sprintf(error_mensaje, "no se esperaba encontrar la expresion [%s]", yytext);
				  yyerror(error_mensaje);
				}
%%



void yyerror(const char * mensaje) {
	char error[1001];
	char aux_msj[1000];
	if(strcmp(mensaje, "syntax error") == 0)
		strcpy(aux_msj, "sintaxis incorrecta");
	else 
		strcpy(aux_msj, mensaje);
	sprintf(error, "\n\n>>>> FIN INESPERADO: linea [%d] del programa <<<<\nERROR: %s\n", 	yylineno, aux_msj);
	printf(error);
	exit(1);

}

void valida_integer(){
	long entero = atoi(yytext);
	if(entero > INTMAX || entero < INTMIN) 
	{		
		printf("Constante INTEGER fuera de rango [%s]\n", yytext);
    	system("Pause");
    	exit(-2);
	}
}

void valida_float() {
    if( strlen(yytext) <= LEN_FLOAT )
        if( atof(yytext) < 0  || atof(yytext) > FLT_MAX_32b )
        {
        	printf("Constante FLOAT fuera de rango [%s]\n", yytext);
    		system("Pause");
    		exit(-2);
        } 
 }

void valida_string() {
    if( strlen(yytext) > LEN_LEXEMA )
    {  
      printf("Constante STRING excede limite de caracteres [%s]\n", yytext);
      system("Pause");
      exit(-3);
    }

}


void valida_id() {
    if( strlen(yytext) > LEN_ID)
    {
	    printf("El ID excede límite de caracteres.Debe tener una longitud maxima de 15 [%s]\n", yytext);
	    system("Pause");
	    exit(-4);
    }
}


void valida_int_bin() {
	if( strlen(yytext) > LEN_INT_BIN )
	{
		printf("Constante INTEGER binaria fuera de rango [%s]\n", yytext);
    	system("Pause");
    	exit(-2);
	}
}


void valida_int_hexa() {
	if( strlen(yytext) > LEN_INT_HEXA )
	{
		printf("Constante INTEGER hexadecimal fuera de rango [%s]\n", yytext);
    	system("Pause");
    	exit(-2);
	}
}
