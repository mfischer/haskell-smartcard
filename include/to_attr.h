#ifndef TO_ATTR_H
#define TO_ATTR_H

#include <wintypes.h>

ULONG
to_attr (ULONG class, ULONG tag)
{
	return SCARD_ATTR_VALUE (class, tag);
}

#endif /* TO_ATTR_H */
