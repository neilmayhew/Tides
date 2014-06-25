#include "TCDAux.h"

// Work around struct-by-value

void get_tide_db_header_(DB_HEADER_PUBLIC* hdr)
{
    *hdr = get_tide_db_header();
}
