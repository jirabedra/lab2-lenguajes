int foo(string a) {
  return 0;
}

int foo2(double a) {
  return 0;
}

int main() {
  foo("AS");
  foo("2.5");
  foo("str");

  foo2(1.0);
  foo2(2.3);

  return 0;
}
