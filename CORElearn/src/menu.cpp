/********************************************************************
*
*   Name:    		 module MENU
*
*   Description:      deals with menus
*
*********************************************************************/


#include "general.h"
#include "error.h"
#include "menu.h"


#if !defined(R_PORT)
#include <cstdio>
#include <cstring>
const int ScreenWidth = 80 ;

using namespace std ;

int textMenu(const char* Title, char const* Item[],int NoItems)
{

   int MaxLen = (int)strlen(Title) ;
    int i, pos ;
    for( i = 0 ; i < NoItems ; i++)
       if ((pos = (int)strlen(Item[i])) > MaxLen)
          MaxLen = pos ;

    int tablen = (ScreenWidth - MaxLen - 4) / 2 ;
    char *tab = new char[tablen + 1] ;
    for (i = 0 ; i < tablen ; i++)
       tab[i] = ' ' ;
    tab[i] = '\0' ;

    printf("\n%s%s\n%s",tab, Title, tab) ;
    pos = (int)strlen(Title) ;
    for (i = 0 ; i < pos ; i++)
      printf("-") ;
    printf("\n") ;

    i = 0 ;
    while (i<NoItems)
    {
       if (i<9)
         printf(" ") ;
       printf("%s%d. %s\n", tab, (i+1), Item[i]) ;
       i++ ;
    }
    delete [] tab ;

    int choice, nread ;
    do {
       printf(">") ;
       fflush(stdout) ;
       nread = scanf("%d", &choice) ;
       if (nread == 0)
		   scanf("%*s") ;
       else if (nread == EOF)
         return -1 ;
    } while (choice<1 || choice > NoItems) ;

    return choice ;

}
#endif // if !defined(R_PORT)
