/*
** bison doesn't put these into pbrill.h
*/

extern	int		brillyyparse(void);
extern	const char	*brillyyfile;
extern	int		brillyylinenum;

/*
** flex doesn't put this into lbrill.h (it doesn't even create lbrill.h)
*/

extern	int		brillyylex(void);
extern	FILE		*brillyyin;

/*
** stdio.h defines this only with some options, not with others
*/

extern	int		fileno(FILE *);
