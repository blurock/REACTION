%%
/*  FILE     sturm2teximain.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM function comments to texinfo
**    descriptions -- main C function.
**
**  REFERENCES
**    Based on Wolfgang Stoecher's previous implementation.
**
**  COPYRIGHT (C) 1995  STURM Project, RISC Linz
*/

#include <stdio.h>
#include <string.h>
#include <time.h>


/*
** MAIN PROGRAM
*/

/*f usage(name) . . . . . . . . . . . . . . . .  print out options of program
**
**  DESCRIPTION
**    name: program name
**
*/
void
usage(const char *name)
{
  fprintf (stderr, "Usage: %s [options] [file]*

Options:
  [-h]     help (show this usage)
  [-s N]   set max. number of subst. per file to N (constraint of \"sed\")
           default: %d
  [-c]     complete file (inclusive header, file names)
  [-p]     pipe output to stdout, otherwise write to section output files
           (default name: \"%s\")

  If there are no [file] parameters, input is read from stdin.\n",
	   name, MAX_SUBST, OUTFILE_NAME);
}


/*F main (argc, argv) . . . . . . . . . . . . . . . . . . . . .  main routine
**
**  DESCRIPTION 
**    argc:
**    argv: standard arguments to "main()".
*/
int
main(int argc, char **argv)
{
  int   i, arg;
  int   complete_file = 0;
  long  now;
  char* today;

  now = time(0);
  today = ctime(&now);

  ProgramName = strrchr (argv[0], '/');
  if (ProgramName == NULL) {    /* no pathname */
    ProgramName = argv[0];
  } else {
    ProgramName++;
  }

  /* 1. handle options */
  for (arg=1; arg<argc; arg++) {
    if (argv[arg][0] != '-')
      break;
    switch (argv[arg][1]) {
    case 'h':
      usage(ProgramName);
      return 0;
    case 'c':
      complete_file = 1;
      break;
    case 'p':
      pipe_output = 1;
      break;
    case 's':
      arg++;
      if (arg < argc)
	Max_Subst = atoi(argv[arg]);
      else {
	usage(ProgramName);
	return 1;
      }
      break;
    default:
      usage(ProgramName);
      return 1;
    }
  }

  /* 2. output file */
  if (!pipe_output) {
    if (freopen(OUTFILE_NAME, "w", stdout) == NULL) {
      fprintf(stderr, "%s: Can't open file \"%s\" for writing!\n",
	      ProgramName, OUTFILE_NAME);
      return 1;
    }
  }

  /* 3. produce output */
  printf("
@c
@c This file was automatically produced at %.24s by
@c %s", today, ProgramName);
  for (i = 1; i < argc; i++) {
    printf(" %s", argv[i]);
  }
  puts("\n@c");
  if (complete_file) {
    puts("\\input texinfo   @c -*-texinfo-*-\n@setfilename sturm2texi.info");
  }

  yyin = stdin;
  do {
    if (arg < argc) {
      yyin = fopen(argv[arg], "r");
      if (yyin == NULL) {
	fprintf (stderr, "%s: Can't open file \"%s\" for reading\n",
		 ProgramName, argv[arg]);
	return 1;
      }
    }
    arg++;
  }
  while (yylex() == 0 && yyin != stdin && arg < argc);

  if (complete_file)  puts("\n@bye");

  return 0;
}
