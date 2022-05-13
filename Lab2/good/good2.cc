double main(){
    { 
      int i = 2 ;
      {
	int k = 3 ;
      }
      i++ ;
      int k = 10;
      return i; // safe implicit cast
    }
}
