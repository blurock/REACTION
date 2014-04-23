%%
/*  FILE     package2teximain.l
**  PACKAGE  Maintenance
**  AUTHOR   Andreas Neubacher
**
**  CONTENT
**    (f)lex program that translates STURM package and class comments to texinfo
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
  [-c]     complete file (inclusive header, file names, use with \"-p\" only)
  [-p]     pipe output to stdout, otherwise write to overview and class output
           files (default names: \"%s\", \"%s\")

  If there are no [file] parameters, input is read from stdin.\n",
	   name, OFILE_NAME, CFILE_NAME);
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
  int   arg;
  int   complete_file = 0;
  long  now;

  now = time(0);
  today = ctime(&now);

  ProgramName = strrchr(argv[0], '/');
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
    default:
      usage(ProgramName);
      return 1;
    }
  }

  /* 2. produce output */
  if (pipe_output && complete_file) {
    puts("\\input texinfo   @c -*-texinfo-*-\n@setfilename package2texi.info");
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

  if (complete_file && pipe_output)  puts("\n@bye");

  return 0;
}
