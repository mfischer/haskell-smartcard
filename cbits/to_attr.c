#include <reader.h>
#include <wintypes.h>

ULONG
to_attr (ULONG class, ULONG tag)
{
	return SCARD_ATTR_VALUE (class, tag);
}
