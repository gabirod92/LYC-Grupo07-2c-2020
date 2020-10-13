%{
	#include <stdio.h>
	#include <stdbool.h>
	#include <string.h>
	#include <stdarg.h>
	#include <math.h>
	#include <stdlib.h>
	#include <errno.h>
	#include "y.tab.h"

	#define TAM 35
	#define TODO_BIEN 1
	#define SIN_MEMORIA 0
	#define DATO_DUPLICADO 0

	typedef struct
	{
			char clave[TAM];
			char tipodato[TAM];
			char valor[TAM];
			char longitud[TAM];
	} info_t;

	typedef struct sNodo
	{
			info_t info;
			struct sNodo *sig;
	} nodo_t;

	typedef nodo_t *lista_t;

	void crear_lista(lista_t *p);
	int insertar_en_orden(lista_t *p, info_t *d);
	int sacar_repetidos(lista_t *p, info_t *d, int (*cmp)(info_t*d1, info_t*d2), int elimtodos);
	void guardar_lista(lista_t *p, FILE *arch);
	int comparar(info_t*d1, info_t*d2);
	void clear_ts();
	void crear_ts(lista_t *l_ts);
	int insertar_en_ts(lista_t *l_ts, info_t *d);
	char *guion_cadena(char cad[TAM]);

	lista_t l_ts;
	info_t d;
	char cadena[TAM+1];

	FILE *yyin;
	extern void yyerror(const char *mensaje);
	extern char error_mensaje[1000];
	extern int yylex(void);
	extern char *yytext;
        extern int yylineno;

        char ids[40][40];
        int cant_ids = 0;

        char tipos[40][40];
        int cant_tipos = 0;

        int i; 
        
%}


%start programa

%token MAXIMO	
%token DIM			
%token AS				
%token BEGINP		
%token ENDP			
%token FLOAT			
%token INTEGER
%token STRING
%token ID	
%token IF		
%token THEN		
%token ELSE			
%token ENDIF			
%left AND   	 
%left OR        	
%token NOT      		
%token WHILE     	
%token ENDWHILE     		
%token PUT 			
%token GET 		
%token COMP_IGUAL
%token COMP_DISTINTO
%token COMP_MENOR 
%token COMP_MAYOR 
%token COMP_MENORIGUAL 
%token COMP_MAYORIGUAL 
%left OP_SUMA OP_RESTA
%left OP_MULTIPLICACION OP_DIVISION 
%token PARENT_ABRE 
%token PARENT_CIERRA 
%token CORCH_ABRE 
%token CORCH_CIERRA
%token LLAVE_ABRE 
%token LLAVE_CIERRA
%token COMA 
%token PUNTO_Y_COMA
%token ASIG 
%token CONSTANTE_ENTERA
%token CONSTANTE_REAL
%token CONSTANTE_STRING
%token CONSTANTE_ENT_HEXA
%token CONSTANTE_ENT_BIN


%%

	programa:
definicion_de_variables {printf("Definicion de variables. \n");} bloque_de_sentencias 
		;

	definicion_de_variables:	setencias_de_declaraciones 
								|
		;
	setencias_de_declaraciones:	setencias_de_declaraciones setencia_declaracion
							  | setencia_declaracion
		;
	setencia_declaracion:
		DIM CORCH_ABRE lista_ids CORCH_CIERRA AS CORCH_ABRE lista_tipos CORCH_CIERRA
                {
                    if (cant_ids != cant_tipos)
                    {
                        printf ("Error sintáctico en linea %d\n", yylineno);
                        printf ("Las listas de variables y tipos deben coincidir");
                        printf ("en cantidad de elementos\n");
                        exit (1);
                    }

                    for (i = 0; i < cant_ids; i++)
                    {
                        strcpy(d.clave, ids[i]);
                        strcpy(d.tipodato, tipos[i]);                        
                        insertar_en_ts(&l_ts, &d);
                    }
                        
                }
		;
	lista_ids : 
		lista_ids COMA ID {
	        strcpy (ids[cant_ids], yytext);
                cant_ids++;}
		| ID {
                strcpy (ids[cant_ids], yytext);
                cant_ids++;
		}
		;

	lista_tipos : 
		lista_tipos COMA tipo 
		| tipo 
		;

	tipo : 
               INTEGER {strcpy (tipos[cant_tipos], "INTEGER"); cant_tipos++;}
             | FLOAT {strcpy (tipos[cant_tipos], "FLOAT"); cant_tipos++;}
             | STRING {strcpy (tipos[cant_tipos], "STRING");cant_tipos++;}
	 	;
	bloque_de_sentencias :	bloque_de_sentencias  sentencia
						 |	sentencia
		;
      sentencia 	: iteracion {printf("Iteracion WHILE. \n");}
		  		| decision  
		  		| salida  PUNTO_Y_COMA {printf("Salida PUT. \n");}
                                | entrada PUNTO_Y_COMA {printf("Entrada GET. \n");}
                                | asignacion PUNTO_Y_COMA {printf("Asignacion. \n");}
		  		| max 
		  
		;
	iteracion	: WHILE PARENT_ABRE condicion PARENT_CIERRA LLAVE_ABRE bloque_de_sentencias LLAVE_CIERRA 
		;

	decision	: IF PARENT_ABRE condicion PARENT_CIERRA LLAVE_ABRE bloque_de_sentencias continuacion_if
		;
continuacion_if	: LLAVE_CIERRA { printf("IF simple. \n");}
| LLAVE_CIERRA ELSE LLAVE_ABRE bloque_de_sentencias LLAVE_CIERRA { printf("IF con ELSE. \n");}
		;
	condicion	: comparacion
				| comparacion AND comparacion 
		  		| comparacion OR comparacion   		
		;
	comparacion : expresion op_comparacion expresion
				| NOT PARENT_ABRE expresion op_comparacion expresion PARENT_CIERRA
		;
	op_comparacion	: COMP_IGUAL
			   		| COMP_MAYOR
			   		| COMP_MENOR
			   		| COMP_MAYORIGUAL
			   		| COMP_MENORIGUAL
			   		| COMP_DISTINTO
		;
	salida		: PUT  ID  
				| PUT CONSTANTE_STRING 
		;
	entrada		: GET ID 
		;
	asignacion	: ID ASIG expresion 
		;	
max			: MAXIMO PARENT_ABRE lista_de_expresiones PARENT_CIERRA { printf("Funcion Maximo. \n");}
		;
	
	lista_de_expresiones: lista_de_expresiones COMA expresion
						| expresion
		;
	expresion	: expresion OP_SUMA termino
				| expresion OP_RESTA termino
				| termino
				| max
		;
	termino		: termino OP_MULTIPLICACION factor
				| termino OP_DIVISION factor
				| factor
		;
	factor		: ID
				| CONSTANTE_ENTERA {
					strcpy(d.clave, guion_cadena(yytext));
					strcpy(d.valor, yytext);
					strcpy(d.tipodato, "const Integer");
					insertar_en_ts(&l_ts, &d);
					}
				|CONSTANTE_ENT_BIN {
					printf("Numero bin. \n");
					strcpy(d.clave, guion_cadena(yytext));
					strcpy(d.valor, yytext);
					strcpy(d.tipodato, "const Int Bin");
					insertar_en_ts(&l_ts, &d);
					}
				|CONSTANTE_ENT_HEXA {
					printf("Numero hexa. \n");
					strcpy(d.clave, guion_cadena(yytext));
					strcpy(d.valor, yytext);
					strcpy(d.tipodato, "const Int Hexa");
					insertar_en_ts(&l_ts, &d);
					}
				| CONSTANTE_REAL {
					strcpy(d.clave, guion_cadena(yytext));
					strcpy(d.valor, yytext);
					strcpy(d.tipodato, "const Float");
					insertar_en_ts(&l_ts, &d);
					}
				| CONSTANTE_STRING {
					strcpy(d.clave, guion_cadena(yytext));
					strcpy(d.valor, yytext);
					strcpy(d.tipodato, "const String");
					sprintf(d.longitud, "%d", (int) strlen(yytext)-2);
                                        printf ("esta es la constante string %s y esta su longitud %s", yytext, d.longitud);
					insertar_en_ts(&l_ts, &d);
					}
				| PARENT_ABRE expresion PARENT_CIERRA
		;	
%%

int main(int argc, char *argv[]) {
	printf("\n");
	printf("==============================================================\n");
	printf("analisis-comienza\n");
	printf("==============================================================\n");
	if ((yyin = fopen(argv[1], "rt")) == NULL) {
		printf("ERROR: abriendo archivo [%s]\n", argv[1]);
	} else {
		clear_ts();
		crear_lista(&l_ts);
		yyparse();
		fclose(yyin);
		crear_ts(&l_ts);
	}
	printf("==============================================================\n");
	printf("Analisis-finalizo\n");
	printf("==============================================================\n");
	return 0;
}	 	

void guardar_lista(lista_t *p, FILE *arch) {
	// titulos
	fprintf(arch,"%-35s %-16s %-35s %-35s", "NOMBRE", "TIPO DE DATO", "VALOR", "LONGITUD");
	// datos
	while(*p) {
		fprintf(arch,"\n%-35s %-16s %-35s %-35s", (*p)->info.clave, (*p)->info.tipodato, (*p)->info.valor, (*p)->info.longitud);
		p=&(*p)->sig;
	}
}


void crear_lista(lista_t *p) {
    *p=NULL;
}

int sacar_repetidos(lista_t *p, info_t *d, int (*cmp)(info_t*d1, info_t*d2), int elimtodos) {
	nodo_t*aux;
	lista_t*q;

	while(*p) {
		q=&(*p)->sig;
		while(*p && *q) {
			if(cmp(&(*p)->info,&(*q)->info)==0) {
				aux=*q;
				*q=aux->sig;
				free(aux);
			} else
				q=&(*q)->sig;
		}
		p=&(*p)->sig;
	}

	return TODO_BIEN;
}

int insertar_en_orden(lista_t *p, info_t *d) {
	nodo_t*nue;
	while(*p && comparar(&(*p)->info,d)>0)
			p=&(*p)->sig;

	if(*p && (((*p)->info.clave)-(d->clave))==0) {
		(*p)->info=(*d);
		return DATO_DUPLICADO;
	}

	nue=(nodo_t*)malloc(sizeof(nodo_t));
	if(nue==NULL)
			return SIN_MEMORIA;

	nue->info=*d;
	nue->sig=*p;
	*p=nue;

	return TODO_BIEN;
}

int comparar(info_t *d1, info_t *d2) {
	return strcmp(d1->clave,d2->clave);	
}

// limpiar una ts de una ejecución anterior
void clear_ts() {
	FILE *arch=fopen("ts.txt","w");
	fclose(arch);
}

void crear_ts(lista_t *l_ts) {
//	info_t aux;
	FILE *arch=fopen("ts.txt","w");
	printf("\n");
	printf("creando tabla de simbolos...\n");
	guardar_lista(l_ts, arch);
	fclose(arch);
	printf("tabla de simbolos creada\n");
}

int insertar_en_ts(lista_t *l_ts, info_t *d) {
	insertar_en_orden(l_ts,d);
	sacar_repetidos(l_ts,d,comparar,0);
	strcpy(d->clave,"\0");
	strcpy(d->tipodato,"\0");
	strcpy(d->valor,"\0");
	strcpy(d->longitud,"\0");
        return 0;
}


char *guion_cadena(char cad[TAM]) {
	char guion[TAM+1]="_" ;
	strcat(guion,cad);
	strcpy(cadena,guion);
	return cadena;
}
