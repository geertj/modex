extern void initx( void);
extern void closex( void);
extern void setapage( int page);
extern void setastart( int offset);
extern void setvpage( int page);
extern void setvstart( int offset);
extern void fpoly( unsigned int nrvertices, void *pvertices, char color);
extern void setpal( int firstdac, int nrdacs, void *pal);
extern void getpal( int firstdac, int nrdacs, void *pal);
extern void fadepal( void *topal);
extern void putpixel( int x, int y, int color);
extern void hblit( void *buf, int y);

