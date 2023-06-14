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
    data = (double *)alloca(10*sizeof(double));
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
