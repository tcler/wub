# Interface to the "zlib" compression library

package provide zlib 0.10
package require critcl

critcl::clibraries -lz

critcl::ccode {
  #include <zlib.h>
}

critcl::ccommand zlib {dummy ip objc objv} {
  int e = TCL_OK, index, dlen, wbits = -MAX_WBITS;
  long flag;
  Byte *data;
  z_stream stream;
  Tcl_Obj *obj = Tcl_GetObjResult(ip);

  static char* cmds[] = {
    "adler32", "crc32", "compress", "deflate", "decompress", "inflate", NULL,
  };

  if (objc < 3 || objc > 4) {
    Tcl_WrongNumArgs(ip, 1, objv, "option data ?...?");
    return TCL_ERROR;
  }

  if (Tcl_GetIndexFromObj(ip, objv[1], cmds, "option", 0, &index) != TCL_OK ||
      objc > 3 && Tcl_GetLongFromObj(ip, objv[3], &flag) != TCL_OK)
    return TCL_ERROR;

  data = Tcl_GetByteArrayFromObj(objv[2], &dlen);

  switch (index) {

    case 0: /* adler32 str ?start? -> checksum */
      if (objc < 4)
	flag = (long) adler32(0, 0, 0);
      Tcl_SetLongObj(obj, (long) adler32((uLong) flag, data, dlen));
      return TCL_OK;

    case 1: /* crc32 str ?start? -> checksum */
      if (objc < 4)
	flag = (long) crc32(0, 0, 0);
      Tcl_SetLongObj(obj, (long) crc32((uLong) flag, data, dlen));
      return TCL_OK;
      
    case 2: /* compress data ?level? -> data */
      wbits = MAX_WBITS;
    case 3: /* deflate data ?level? -> data */
      if (objc < 4)
	flag = Z_DEFAULT_COMPRESSION;

      stream.avail_in = (uInt) dlen;
      stream.next_in = data;

      stream.avail_out = (uInt) dlen + dlen / 1000 + 12;
      Tcl_SetByteArrayLength(obj, stream.avail_out);
      stream.next_out = Tcl_GetByteArrayFromObj(obj, NULL);

      stream.zalloc = 0;
      stream.zfree = 0;
      stream.opaque = 0;

      e = deflateInit2(&stream, (int) flag, Z_DEFLATED, wbits,
			      MAX_MEM_LEVEL, Z_DEFAULT_STRATEGY);
      if (e != Z_OK)
	break;

      e = deflate(&stream, Z_FINISH);
      if (e != Z_STREAM_END) {
	deflateEnd(&stream);
	if (e == Z_OK) e = Z_BUF_ERROR;
      } else
	e = deflateEnd(&stream);
      break;
      
    case 4: /* decompress data ?bufsize? -> data */
      wbits = MAX_WBITS;
    case 5: /* inflate data ?bufsize? -> data */
    {
      if (objc < 4)
	flag = 16 * 1024;

      for (;;) {
	stream.zalloc = 0;
	stream.zfree = 0;

	/* +1 because ZLIB can "over-request" input (but ignore it) */
	stream.avail_in = (uInt) dlen +  1;
	stream.next_in = data;

	stream.avail_out = (uInt) flag;
	Tcl_SetByteArrayLength(obj, stream.avail_out);
	stream.next_out = Tcl_GetByteArrayFromObj(obj, NULL);

	/* Negative value suppresses ZLIB header */
	e = inflateInit2(&stream, wbits);
	if (e == Z_OK) {
	  e = inflate(&stream, Z_FINISH);
	  if (e != Z_STREAM_END) {
	    inflateEnd(&stream);
	    if (e == Z_OK) e = Z_BUF_ERROR;
	  } else
	    e = inflateEnd(&stream);
	}

	if (e == Z_OK || e != Z_BUF_ERROR) break;

	Tcl_SetByteArrayLength(obj, 0);
	flag *= 2;
      }

      break;
    }
  }

  if (e != Z_OK) {
    Tcl_SetResult(ip, (char*) zError(e), TCL_STATIC);
    return TCL_ERROR;
  }

  Tcl_SetByteArrayLength(obj, stream.total_out);
  return TCL_OK;
}

if {[info exists pkgtest] && $pkgtest} {

  proc zlib_try {} {
    set text "Hello world, world, world, world, world!"
    puts "adler = [format %08x [zlib adler32 $text]]"
    puts "crc32 = [format %08x [zlib crc32 $text]]"
    set small [zlib compress $text]
    binary scan $small H* hex
    puts "compress = $hex"
    puts "restored = [zlib decompress $small]"
    set small [zlib deflate $text]
    binary scan $small H* hex
    puts "deflated = $hex"
    puts "restored = [zlib inflate $small]"
  }

  zlib_try
}
