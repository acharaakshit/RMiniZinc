#define PKG_MZN_PARSE 50

#ifndef PKG_MZN_PARSE
  #define MZN_1 10
#elif ~(~PKG_MZN_PARSE + 0) == 0 && ~(~PKG_MZN_PARSE + 1) == 1
    #define MZN_2 20
#else
    #define MZN_PARSE 30
#endif


#define PKG_MZN_EVAL 

#ifndef PKG_MZN_EVAL
  #define MZN_3 10
#elif ~(~PKG_MZN_EVAL + 0) == 0 && ~(~PKG_MZN_EVAL + 1) == 1
    #define MZN_4 20
#else
    #define MZN_EVAL 30
#endif
