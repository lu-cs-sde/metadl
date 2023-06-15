#include "std_testcase.h"

void printDoubleLine(double);

void CWE457_Use_of_Uninitialized_Variable__double_01_bad()
{
    double data;
    /* POTENTIAL FLAW: Don't initialize data */
    ; /* empty statement needed for some flow variants */
    /* POTENTIAL FLAW: Use data without initializing it */
    printDoubleLine(data);
}


void CWE457_Use_of_Uninitialized_Variable__double_02_bad()
{
    double data;
    if(1)
    {
        /* POTENTIAL FLAW: Don't initialize data */
        ; /* empty statement needed for some flow variants */
    }
    if(1)
    {
        /* POTENTIAL FLAW: Use data without initializing it */
        printDoubleLine(data);
    }
}

void CWE457_Use_of_Uninitialized_Variable__double_array_alloca_no_init_16_bad()
{
    double * data;
    data = (double *)ALLOCA(10*sizeof(double));
    while(1)
    {
        /* POTENTIAL FLAW: Don't initialize data */
        ; /* empty statement needed for some flow variants */
        break;
    }
    while(1)
    {
        /* POTENTIAL FLAW: Use data without initializing it */
        {
            int i;
            for(i=0; i<10; i++)
            {
                printDoubleLine(data[i]);
            }
        }
        break;
    }
}

void printIntLine(int);
static void goodG2B()
{
    int * data;
    /* FIX: Initialize data */
    /* initialize both the pointer and the data pointed to */
    data = (int *)malloc(sizeof(int));
    if (!data) {exit(-1);}
    *data = 5;
    /* POTENTIAL FLAW: Use data without initializing it */
    printIntLine(*data);
}


static void goodB2G()
{
    int * data;
    data = (int *)ALLOCA(10*sizeof(int));
    /* POTENTIAL FLAW: Don't initialize data */
    ; /* empty statement needed for some flow variants */
    /* FIX: Ensure data is initialized before use */
    {
        int i;
        for(i=0; i<10; i++)
        {
            data[i] = i;
        }
    }
    {
        int i;
        for(i=0; i<10; i++)
        {
            printIntLine(data[i]);
        }
    }
}


void CWE457_Use_of_Uninitialized_Variable__int_array_declare_no_init_01_bad()
{
    int * data;
    int dataUninitArray[10];
    data = dataUninitArray;
    /* POTENTIAL FLAW: Don't initialize data */
    ; /* empty statement needed for some flow variants */
    /* POTENTIAL FLAW: Use data without initializing it */
    {
        int i;
        for(i=0; i<10; i++)
        {
            printIntLine(data[i]);
        }
    }
}
