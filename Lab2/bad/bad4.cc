bool f(){
    { 
      int i = 2 ;
      {
	int k = 3 ;
      }
      i++ ;
      int k = 10;
      return i; // does not returns a boolean
    }
}
