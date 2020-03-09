
/* takes a function pointer an invokes it */
void invoke(void (*f)(void))
{
	f();
}

/* prints a simple message */
void run(void)
{
	puts("inside run");
}
