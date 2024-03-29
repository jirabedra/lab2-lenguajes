void printInt(int x) { }
void printDouble(double x) { }

int
main() {
	printInt(fac(10));
	printInt(rfac(10));
	printInt(mfac(10));
        printInt(ifac(10));
        double r ; // just to test blocks 
	{
	  int n = 10;
	  int r = 1;
	  while (n>0) {
	    r = r * n;
	    n = n - 1;
	  }
	  printInt(r);
	}
	printDouble(dfac(10.0));
        return 0 ;
}

int 
fac(int a) {
	int r;
	int n;

	r = 1;
	n = a;
	while (n > 0) {
		r = r * n;
		n = n - 1;
	}
	return r;
}

int
rfac(int n)
{
	if (n == 0)
		return 1;
	else
		return n * rfac(n-1);
}

int
mfac(int n)
{
	if (n == 0)
		return 1;
	else
		return n * nfac(n-1);
}

int
nfac(int n)
{
	if (n != 0)
		return mfac(n-1) * n;
	else
		return 1;
}

double
dfac(double n)
{
	if (n == 0.0)
		return 1.0;
	else
		return n * dfac(n-1.0);
}

double ifac(double n) { return ifac2f(1.0,n); }

double ifac2f(double l, double h) {
        if (l == h)
                return l;
        else if (l > h)
                return 1.0;
	else {
		double m;
		m = (l+h) / 2;
		return ifac2f(l,m) * ifac2f(m+1,h);
	}
}
