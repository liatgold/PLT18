/* demo demonstraiting the calling of one function from main
by Gabriel Brown */

int main(int a){
	a = foo(a);
	return a;
}

int foo(int b){
	b = b+4;
	return b;
}
