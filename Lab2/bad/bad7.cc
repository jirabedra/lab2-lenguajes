int f(int a, double b,int c){
  bool d = a <= a && a >= a || b < b || b > b || c != a; // check  boolean expresions 
  int i = 2 ;
  b = d;
  d = b;   // bad assignment, can not cast in a secure way double to bool
}
