
// Single line comment

/* Multi
	Line
    	Comment */

#tryinclude <SomeInclude>

#pragma tabsize 4

new SomeNewVariable;
stock SomeStockVariable;
public SomePublicVariable;
static SomeStaticVariable;

enum SomeEnum
{
	Integer,
    bool:Boolean,
    Float:Floating,
    String[128],
    PackedString[128 char],
};

native SomeNativeFunction(SomeParameter);

forward SomeCallback(SomeParameter);
public SomeCallback(SomeParameter)
{
	assert(0);

    sleep(100);

    for(;;)
    {
    	// Do something
        break;
    }
:here
    new i = 0;
    do {
    	if(i == -1)
        	continue;

		if(i == 10)
        	break;
        i++;
    } while(true);

	switch(i)
    {
    case 0: i = i++;
	case 1: i = (i != 0) ? (10) : (20)
    default: i = 1;
    }

    if i == 0 *then
    	exit;
    else
    	goto here;

    new l = 0, r = 1;
    if tagof(l) == tagof(r) *then
    *begin
    	if(!defined(l))
			exit;
    *end

	return 1;
}

