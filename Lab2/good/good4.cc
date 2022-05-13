double f(){
    { 
      int i = 2 ;
      {
	int k = 3 ;
	i = k + 3 ; // can use a variable in an outter context
      }
      int k = 10; // can redefine k
    }
}
