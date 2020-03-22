
// Single line comment

/* Multi
	Line
	    Comment */
	    
#tryinclude "SomeInclude"

#define SomeMacro (1)
#if defined SomeMacro
	#if 0 < SomeMacro
	    #assert SomeMacro is bigger than 0!
	#endif
#else
	#assert No SomeMacro defined!
#endif

#if defined PlzDontCallMe
	assert
	char
	defined
	sizeof
	state
	tagof
	break
	case
	continue
	default
	do
	else
	exit
	for
	goto
	if
	return
	sleep
	switch
	while
	const
	enum
	forward
	native
	new
	operator
	public
	static
	stock
    true
    false
#endif

#pragma tabsize 4
#undef SomeMacro

native SomeSomeNative(const SomeParam[]);
native SomeNative(const SomeParam[]) = SomeSomeNative;

enum SomeEnum
{
	SOME_MEMBER_1 = 0, // Some comment here
	SOME_MEMBER_2,
	SOME_MEMBER_3 = 4,
	SOME_MEMBER_4
};

stock SomeArrayStockVar[10];
static Float:SomeFloatStaticVar = Float:123.456;
new bool:SomeBoolNewVar = false;
public SomePublicVar;

entry()
{
SomeJump:
	SomePublicVar = sizeof(SomeArrayStockVar);
	
	while(SomePublicVar)
	{
	    SomePublicVar--;
	}
	
	for(new x = 0; x <= 10; x++)
	{
	    SomePublicVar = SomePublicVar * 2;
	    if(x == 5)
			sleep(10);
		else if(x == 9)
		    break;
	}
	
	SomeArrayStockVar[0] = SomePublicVar / 2;
	
	switch(SomeArrayStockVar)
	{
	case 1 .. 100: { SomeFloatStaticVar = 654.321 }
	default: {}
	}
	
	if(!SomeBoolNewVar)
	{
	    SomeBoolNewVar = true;
	    goto SomeJump;
	}
}

forward SomeCallback(const SomeParam);
public SomeCallback(const SomeParam)
{
	assert(SomeParam);
	return;
}

