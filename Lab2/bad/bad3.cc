int f(){
    { 
      double i = 2 ;
      {
	int k = 3 ;
      }
      i++ ;
      double k = 10;
      return i; // bad implicit cast
    }
}
