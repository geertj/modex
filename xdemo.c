/* NOTE: This is a _VERY_ lame demo, it only shows the some of the        */
/*      possiblilities of the graphic routines.                           */

#include <stdio.h>
#include <stdlib.h>
#include "modex.h"

struct POINT
{ int x;
  int y;
};

typedef struct POINT point;

point triangle[3];

int random( int max)
{ float a;
  a = (float) rand() * max / RAND_MAX;
  return (int) a;
}


main()
{ int x, y, i;
  char zeropal[768];

  srand( 982);
  for (i=0; i<768; i++)
  { zeropal[i] = 0;
  }

  initx();
  setapage(0);
  setvpage(1);
  sleep( 3);

  for (x=0; x<319; x++)
    for (y=0; y<239; y++)
      putpixel( x, y, x+y);
  setvpage(0);

  for (i=0; i<10000; i++)
  { triangle[0].x = random( 320);
    triangle[0].y = random( 240);
    triangle[1].x = random( 320);
    triangle[1].y = random( 240);
    triangle[2].x = random( 320);
    triangle[2].y = random( 240);

    fpoly(3, &triangle, random( 255));
  }

  sleep( 3);

  fadepal( &zeropal);
  waitvrt();
  closex();

  return 0;
}
