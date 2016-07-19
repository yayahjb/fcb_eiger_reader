//
//  cbf_register_filters.c
//
//  wrapper to register lz4 and bitshuffle filters
//  for fortran code, part of the FCB_EIGER_READER
//  package
//
//  Created by Herbert J. Bernstein on 6/30/16.
//  extracted from eiger2cbf.c by Takanori Nakane
//
//
//
/*======================EIGER2CBF LICENSE ============================
 * eiger2cbf is licensed under the "BSD license", referencing the
 * versions of the BSD license in bishuffle and lz4.  The BSD license
 * version in bitshuffle says:
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
 * IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 *====================================================================*/
 
/*===================FCB_EIGER_READER_LICENSE =========================
 **********************************************************************
 *                                                                    *
 * YOU MAY REDISTRIBUTE THE FCB_EIGER PACKAGE UNDER THE TERMS OF THE  *
 * LGPL IN ADDITION TO THE EIGER2CBF BSD LICENSE                      *
 **********************************************************************/

/************************** LGPL NOTICES ******************************
 *                                                                    *
 * This library is free software; you can redistribute it and/or      *
 * modify it under the terms of the GNU Lesser General Public         *
 * License as published by the Free Software Foundation; either       *
 * version 2.1 of the License, or (at your option) any later version. *
 *                                                                    *
 * This library is distributed in the hope that it will be useful,    *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of     *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  *
 * Lesser General Public License for more details.                    *
 *                                                                    *
 * You should have received a copy of the GNU Lesser General Public   *
 * License along with this library; if not, write to the Free         *
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,    *
 * MA  02110-1301  USA                                                *
 *                                                                    *
 **********************************************************************/


#include "hdf5.h"

extern const H5Z_class2_t H5Z_LZ4;
extern const H5Z_class2_t bshuf_H5Filter;
void cbf_register_filters() {
    H5Zregister(&H5Z_LZ4);
    H5Zregister(&bshuf_H5Filter);
}

