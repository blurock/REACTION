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

 

 
/*F SubstCheck() . . . . . . if necessary, open another "SubstFile"
**
**  DESCRIPTION
**    If the maximal number of substitutions per file is reached,
**    another "SubstFile" is opened.
*/
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
	has_filename = (CmdLineOption( argc, argv, '1', &dummy, &input_name) == GIVEN);
	if (has_filename) {	       /* last argument is input file name */
	  if (freopen (input_name, "r", stdin) == NULL) {
	    fprintf (stderr, "%s: Can't open `%s' for reading\n",
		     program_name, input_name);
	    exit (2);
	  }
	}
  /* 2.1 create output file name */
	out_body_name = "Overview";
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
/*        SubstCheck ();*/		/* takes care of opening a new file */
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












