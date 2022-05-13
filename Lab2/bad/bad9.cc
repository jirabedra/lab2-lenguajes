double f(int a, double b,int c){
  int i = 2;
  if ( a < 2 ) // test if with bad first branch
    c = b; // not secure implicit casting
  else
    i++;
}



