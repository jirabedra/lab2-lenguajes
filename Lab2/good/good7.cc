double g(int a, double b, bool c){
  double i = f(a,b,c) ; // functions applications, this works because we have done a 2 passes compiler !
                        // real c complilers do not do two passes
}

double f(int a, double b,bool c){
  int i = 2 ;
}

