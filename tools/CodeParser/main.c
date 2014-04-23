/*  FILE    main.c
**P PACKAGE Sturm2texi
**A AUTHORS Wolfgang Stoecher
**          Andreas Neubacher
**
**  CONTENT
**  This is a flex input file but should be edited in -*-C++-*- mode
**
**  Sturm2texi: Produce texi files from  C++Sturm.
**  Copyright (C) 1994 SturmProject
**
**  This program is free software; you can redistribute it and/or modify
**  it under the terms of the GNU General Public License as published by
**  the Free Software Foundation; either version 1, or (at your option)
**  any later version.
**
**  This program is distributed in the hope that it will be useful,
**  but WITHOUT ANY WARRANTY; without even the implied warranty of
**  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**  GNU General Public License for more details.
**
*/
 
/*I . . . INCLUDES  . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>
#include <time.h>
#include "CmdLineOption.h"

/*D . . . DEFINES . . . . . . . . . . . . . . . . . . . . . . . . . . . . .  
*/
/*   maximal number of subst. per SubstFile; constraint of "sed" */
#define MAX_SUBST 150

 
/*P . . . PROTOTYPES  . . . . . . . . . . . . . . . . . . . . . . . . . . . 
*/
static
void
SubstCheck (void);

 
/*F SubstOpType(Op, RetPar, NumGlb) . . make a substitution for operator type
**
**  DESCRIPTION
**    Op: name of operator declaration
**    RetPar: Flag, whether value is returned
**    NumGlb: unique number (globally counting all functions/operators) 
**
**    The operator type is written to "SubstFile"
**    as a substitution for `FUNC_REPL_STR'`NumGlb',
**    introducing 'CLASS_REPL_STR', which will be substituted
**    later on.
**
*/
void
SubstOpType (char *OpType, int RetPar, int NumGlb)
{
  SubstCheck ();
  fprintf(SubstFile, "s/%s%dxx/%s %s%dxx/1\n", FUNC_REPL_STR, NumGlb,
	  OpType, CLASS_REPL_STR, NumGlb);
}
 
/*F SubstFuncType(FuncType, NumGlb) . . make a substitution for function type
**
**  DESCRIPTION
**    FuncType: name of function type
**    NumGlb: unique number (globally counting all functions/operators) 
**
**    The function type is written to `SubstFile'
**    as a substitution for `FUNC_REPL_STR'`NumGlb'.
**
*/
void
SubstFuncType (char *FuncType, int NumGlb)
{
  SubstCheck ();
  fprintf(SubstFile, "s/%s%dxx/%s/1\n", FUNC_REPL_STR, NumGlb, FuncType);
}
 
/*F SubstParaType(Parameter, NumGlb) make a substitution for typed parameters
**
**  DESCRIPTION
**    Parameter: Full list of type, name
**    NumGlb: unique number (global counted) 
**
**    The parameter type declaration may consist of one or
**    more identifiers, like const Type_Integer &, which is written to `SubstFile'
**    as a substitution for `PAR_REPL_STR'`NumGlb'.
*/
void
SubstParaType (char *Parameter, int NumGlb)
{
  int Length = strlen (Parameter);
  char *LastToken = Parameter + Length;

  while (strpbrk(--LastToken, " \t\n"))
    *LastToken = '\0';			        /* remove white space at end */
  while (!strchr(" \t\n*&", *(--LastToken)));	/* remove parameter name at end*/
  while (strchr(" \t\n", *LastToken))		/* remove white space at end */
    LastToken--;
  *(LastToken+1) = '\0';
  if (*LastToken == '&') {			/* replace '&' by '\&' */
    *LastToken = '\0';
    strcat(Parameter, "\\&");
  }
  while (strchr(" \t\n", *Parameter))		/* remove white space at beginning */
     Parameter++;

  SubstCheck ();
  fprintf(SubstFile, "s/%s%dxx/%s/1\n", PARA_REPL_STR, NumGlb, Parameter);
}
 
/*F SubstClass(Parameter, NumGlb) . . . . . . make a substitution for a class
**
**  DESCRIPTION
**    Parameter: Full list of type, name
**    NumGlb: unique number (global counted) 
**
**    From the parameter/function type declaration (including the name)
**    the class is extracted:
**    - either as the classname of the functionname (seperated by "::")
**    - or as typename
**    both without template instantiations. The class is written to `SubstFile'
**    as a substitution for `CLASS_REPL_STR'`NumGlb'.
*/
void
SubstClass (char *Parameter, int NumGlb)
{
  int Length = strlen (Parameter);
  char *LastToken = Parameter + Length;
  char *Class;
  int TemplDepth;

  while (strpbrk(--LastToken, " \t\n"))
    *LastToken = '\0';			        /* remove white space at end */
  while (!strchr(" \t\n*&>:", *(--LastToken)));	/* remove para/func name at end*/
  while (strchr(" \t\n&*:", *LastToken))	/* remove space,*,&,: at end */
    LastToken--;
  if (*LastToken == '>') {                      /* remove template type */ 
    LastToken--;
    TemplDepth = 1;
    while (TemplDepth && LastToken != Parameter) {
      if (*LastToken == '>') TemplDepth++;
      if (*LastToken == '<') TemplDepth--;
      LastToken--;
    }
  }

  *(LastToken+1) = '\0';	                /* terminate string */
  Class = LastToken;
  while (!strchr(" \t\n", *(Class)) && 
	 Class != Parameter)		        /* extract class name */
    Class--;

  SubstCheck ();
  fprintf(SubstFile, "s/%s%dxx/%s/1\n", CLASS_REPL_STR, NumGlb, Class);
}

/*f SubstCheck() . . . . . . if necessary, open another "SubstFile"
**
**  DESCRIPTION
**    If the maximal number of substitutions per file is reached,
**    another "SubstFile" is opened.
*/
static
void
SubstCheck (void)
{
  static int NrSubstitutions = -1; /* open new file at beginning */
  static int NrSubstFiles = 0;
  char *sub_name;
  int sub_file;

  if (NrSubstitutions == -1) NrSubstitutions = SubstMax;
  if (++NrSubstitutions > SubstMax) {
    NrSubstitutions = 0;
    sub_name = malloc( strlen( out_body_name ) + 9 );
    if ( sub_name == 0 ) {
      fprintf( stderr, "%s: Virtual memory exhausted\n", program_name );
      exit( 3 );
    }
    sprintf (sub_name,"%s_%d.sub", out_body_name, ++NrSubstFiles);
    sub_file = creat(sub_name, 432);    /* 432: -rw-rw---- */
    if ( sub_file == 0 ) {
      fprintf( stderr, "%s: Can't open `%s' for writing\n",
	       program_name, sub_name );
      exit( 3 );
    }
    SubstFile = fdopen( sub_file, "w" );
  }
}

/*F main (argc, argv) . . . . . . . . . . . . . . . . . . . . .  main routine
**
**  DESCRIPTION 
**    argc:
**    argv: standard arguments to "main()".
*/
main (int argc, char **argv)
{
	int     c;
	int     index;
	int     i;
        int     help;
	int     has_filename;
	char   *input_name;
	char   *output_name;
	char   *info_name;
        char   *sub_name;
	long   now;
	char   *today;
        int    dummy;
        char   *dummy1;
	void   *malloc (size_t);

	input_name = "Standard Input";
	output_name = 0;

	now = time (0);
	today = ctime (&now);

	program_name = strrchr (argv[0], '/');
	if (program_name == NULL) {    /* no pathname */
		program_name = argv[0];
	} else {
		program_name++;
	}

  /* 1. handle options */
   help          = ( CmdLineOption( argc, argv, 'h', &dummy, &dummy1 ) ==
		     GIVEN );
   if (help) usage( program_name );
   complete_file = ( CmdLineOption( argc, argv, 'c', &dummy, &dummy1 ) ==
		     GIVEN );
   piped         = ( CmdLineOption( argc, argv, 'p', &dummy, &dummy1 ) ==
		     GIVEN );
   SubstMax = MAX_SUBST;   
   CmdLineOption( argc, argv, 's', &SubstMax, &dummy1 ); 
   if (SubstMax == NOT_GIVEN) usage( program_name );

  /* 2. prepare names, output-stream/file, replacement file */
  /* 2.0 check input file */
	has_filename = (CmdLineOption( argc, argv, '1', &dummy, &input_name ) 
                        == GIVEN);
	if (has_filename) {	       /* first argument is input file name */
	  yyin = fopen (input_name, "r");
	  if (yyin == NULL) {
	    fprintf (stderr, "%s: Can't open `%s' for reading\n",
		     program_name, input_name);
	    exit (2);
	  }
	}
	else
	  yyin = stdin;
	if (has_filename) {
	  out_body_name = strrchr (input_name, '/');
	  if (out_body_name == 0) {	/* plain filename */
	    out_body_name = input_name;
	  } else {
	    out_body_name++;
	  }
	} else {
	  out_body_name = program_name;
	}
  /* 2.1 create output file name */
	if ((output_name == 0) && !piped) {
		output_name = malloc (strlen (out_body_name) + 6);
		if (output_name == 0) {
			fprintf (stderr, "%s: Virtual memory exhausted\n", program_name);
			exit (3);
		}
		strcpy (output_name, out_body_name);
		strcat (output_name, ".texi");
	}
  /* 2.2 open output file */
	if (!piped) {
		if (freopen (output_name, "w", stdout) == NULL) {
			fprintf (stderr, "%s: Can't open `%s' for writing\n",
				 program_name, output_name);
			exit (3);
		}
	}
  /* 2.3 create info file name */
        info_name = malloc (strlen (out_body_name) + 6);
	if (info_name == 0) {
	  fprintf (stderr, "%s: Virtual memory exhausted\n", program_name);
	  exit (3);
	}
	strcpy (info_name, out_body_name);
	strcat (info_name, ".info");
  /* 2.4 open substitution file */
        /* this file is used for substituting the dummies for Parametertypes */
	/* after parsing in order to avoid preparsing or a second parse */
	/* run */
        SubstCheck ();		/* takes care of opening a new file */
				/* whenever one is 'full' (restriction */
				/* by 'sed') */
  /* 3. produce output */
	printf ("\
@c\n\
@c This file was automatically produced at %.24s by\n\
@c %s", today, program_name);
	for (i = 1; i < argc; i++) {
		printf (" %s", argv[i]);
	}
	if (!has_filename) {
		printf (" (from Standard Input)");
	}
	printf ("\n@c\n");
	if (complete_file) {
		  printf("\\input texinfo   @c -*-texinfo-*-\n\
@setfilename %s\n", info_name);
	}
	yylex ();
	if (complete_file) {
	  if (!piped) {
	    if (freopen (output_name, "a", stdout) == NULL) {
	      fprintf (stderr, "%s: Can't open `%s' for writing\n",
		       program_name, output_name);
	      exit (3);
	    }
	  }
	  printf("\n@bye\n");
	}
	exit (0);
}
 
/*f usage(name) . . . . . . . . . . . . . . . .  print out options of program
**
**  DESCRIPTION
**    name: program name
**
*/
void
usage (const char *name)
{
	fprintf (stderr, "\
Usage: %s [options] [file]\n\n\
Options:\n\
  [-h]     help (show this usage)\n\
  [-sNUM]  set max. number of subst. per file to NUM (constraint of 'sed')\n\
           default: %d\n\
  [-c]     complete file (inclusive header, file names)\n\
  [-p]     piped (output to stdout, otherwise to default output files)\n\n\
  if [file] is not specified, input is read from stdin.\n",\
name, MAX_SUBST);
	exit (1);
}


 
/*E Emacs . . . . . . . . . . . . . . . . . . . . . . . local emacs variables
  Local Variables:
  outline-regexp:     "/\\*[A-Za-z]?"
  eval:               (outline-minor-mode)
  eval:               (hide-body)
  End:
*/
