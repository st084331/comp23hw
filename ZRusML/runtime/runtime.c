#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

typedef struct partial_application
{
    int64_t *fun;
    int total;   // Total number of arguments
    int applied; // Number of arguments applied
    int64_t *args;
} *papp;

static int64_t apply0(papp p)
{
    return ((int64_t(*)())p->fun)();
}

static int64_t apply1(papp p)
{
    return ((int64_t(*)(int64_t))p->fun)(p->args[0]);
}

static int64_t apply2(papp p)
{
    return ((int64_t(*)(int64_t, int64_t))p->fun)(p->args[0], p->args[1]);
}

static int64_t apply3(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2]);
}

static int64_t apply4(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3]);
}

static int64_t apply5(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4]);
}

static int64_t apply6(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5]);
}

static int64_t apply7(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6]);
}

static int64_t apply8(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7]);
}

static int64_t apply9(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8]);
}

static int64_t apply10(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9]);
}

static int64_t apply11(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10]);
}

static int64_t apply12(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11]);
}

static int64_t apply13(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12]);
}

static int64_t apply14(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13]);
}

static int64_t apply15(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14]);
}

static int64_t apply16(papp p)
{
    return ((int64_t(*)(int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t, int64_t))p->fun)(p->args[0], p->args[1], p->args[2], p->args[3], p->args[4], p->args[5], p->args[6], p->args[7], p->args[8], p->args[9], p->args[10], p->args[11], p->args[12], p->args[13], p->args[14], p->args[15]);
}

static int64_t (*applicators[17])(papp) = {
    apply0,
    apply1,
    apply2,
    apply3,
    apply4,
    apply5,
    apply6,
    apply7,
    apply8,
    apply9,
    apply10,
    apply11,
    apply12,
    apply13,
    apply14,
    apply15,
    apply16};

papp new_apply(int64_t *fun, int total)
{
    papp p = (papp)malloc(sizeof(struct partial_application));
    p->fun = fun;
    p->total = total;
    p->applied = 0;
    p->args = (int64_t *)malloc(sizeof(int64_t) * total);
    return p;
}

papp copy_apply(papp p)
{
    papp p2 = (papp)malloc(sizeof(struct partial_application));
    p2->fun = p->fun;
    p2->total = p->total;
    p2->applied = p->applied;
    p2->args = (int64_t *)malloc(sizeof(int64_t) * p->total);
    for (int i = 0; i < p->applied; i++)
    {
        p2->args[i] = p->args[i];
    }
    return p2;
}

int64_t partially_apply(int64_t pointer, int64_t arg)
{
    papp p = (papp)pointer;
    papp new = copy_apply(p);

    new->args[p->applied] = arg;
    new->applied++;
    if (new->applied == new->total)
    {
        int64_t (*fun)(papp) = applicators[new->total];
        int64_t res = fun(new);
        free(new->args);
        free(new);
        return res;
    }
    else if (new->applied > new->total)
    {
        printf("Error: too many arguments applied\n");
        exit(1);
    }

    return (int64_t) new;
}

int64_t create_new_apply(int64_t func, int64_t total)
{
    if (total > 16)
    {
        printf("Error: сannot create a function with more than 16 arguments");
        exit(1);
    }
    int64_t *fun = (int64_t *)func;
    papp new = new_apply(fun, total);
    if (total == 0)
    {
        int64_t (*fun)(papp) = applicators[new->total];
        int64_t res = fun(new);
        free(new->args);
        free(new);
        return res;
    }
    return (int64_t) new;
}

int64_t print_int(int64_t x)
{
    printf("%ld", x);
    return 0;
}

int64_t print_bool(int64_t x)
{
    if (x == 0)
    {
        printf("false");
    }
    else
    {
        printf("true");
    }
    return 0;
}

int64_t print_char(int64_t x)
{
    printf("%c", (char)x);
    return 0;
}