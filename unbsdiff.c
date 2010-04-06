/*-
 * Copyright 2010 Bob Ippolito
 * All rights reserved
 *
 * This is a derivative work of bsdiff by Colin Percival. These
 * changes shall be distributed under the same BSD license as bsdiff, see
 * below.
 * 
 * Copyright 2003-2005 Colin Percival
 * All rights reserved
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted providing that the following conditions 
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <sys/types.h>

#include <err.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <zlib.h>

#define MIN(x,y) (((x)<(y)) ? (x) : (y))

static void split(off_t *I,off_t *V,off_t start,off_t len,off_t h)
{
	off_t i,j,k,x,tmp,jj,kk;

	if(len<16) {
		for(k=start;k<start+len;k+=j) {
			j=1;x=V[I[k]+h];
			for(i=1;k+i<start+len;i++) {
				if(V[I[k+i]+h]<x) {
					x=V[I[k+i]+h];
					j=0;
				};
				if(V[I[k+i]+h]==x) {
					tmp=I[k+j];I[k+j]=I[k+i];I[k+i]=tmp;
					j++;
				};
			};
			for(i=0;i<j;i++) V[I[k+i]]=k+j-1;
			if(j==1) I[k]=-1;
		};
		return;
	};

	x=V[I[start+len/2]+h];
	jj=0;kk=0;
	for(i=start;i<start+len;i++) {
		if(V[I[i]+h]<x) jj++;
		if(V[I[i]+h]==x) kk++;
	};
	jj+=start;kk+=jj;

	i=start;j=0;k=0;
	while(i<jj) {
		if(V[I[i]+h]<x) {
			i++;
		} else if(V[I[i]+h]==x) {
			tmp=I[i];I[i]=I[jj+j];I[jj+j]=tmp;
			j++;
		} else {
			tmp=I[i];I[i]=I[kk+k];I[kk+k]=tmp;
			k++;
		};
	};

	while(jj+j<kk) {
		if(V[I[jj+j]+h]==x) {
			j++;
		} else {
			tmp=I[jj+j];I[jj+j]=I[kk+k];I[kk+k]=tmp;
			k++;
		};
	};

	if(jj>start) split(I,V,start,jj-start,h);

	for(i=0;i<kk-jj;i++) V[I[jj+i]]=kk-1;
	if(jj==kk-1) I[jj]=-1;

	if(start+len>kk) split(I,V,kk,start+len-kk,h);
}

static void qsufsort(off_t *I,off_t *V,u_char *old,off_t oldsize)
{
	off_t buckets[256];
	off_t i,h,len;

	for(i=0;i<256;i++) buckets[i]=0;
	for(i=0;i<oldsize;i++) buckets[old[i]]++;
	for(i=1;i<256;i++) buckets[i]+=buckets[i-1];
	for(i=255;i>0;i--) buckets[i]=buckets[i-1];
	buckets[0]=0;

	for(i=0;i<oldsize;i++) I[++buckets[old[i]]]=i;
	I[0]=oldsize;
	for(i=0;i<oldsize;i++) V[i]=buckets[old[i]];
	V[oldsize]=0;
	for(i=1;i<256;i++) if(buckets[i]==buckets[i-1]+1) I[buckets[i]]=-1;
	I[0]=-1;

	for(h=1;I[0]!=-(oldsize+1);h+=h) {
		len=0;
		for(i=0;i<oldsize+1;) {
			if(I[i]<0) {
				len-=I[i];
				i-=I[i];
			} else {
				if(len) I[i-len]=-len;
				len=V[I[i]]+1-i;
				split(I,V,i,len,h);
				i+=len;
				len=0;
			};
		};
		if(len) I[i-len]=-len;
	};

	for(i=0;i<oldsize+1;i++) I[V[i]]=i;
}

static off_t matchlen(u_char *old,off_t oldsize,u_char *new,off_t newsize)
{
	off_t i;

	for(i=0;(i<oldsize)&&(i<newsize);i++)
		if(old[i]!=new[i]) break;

	return i;
}

static off_t search(off_t *I,u_char *old,off_t oldsize,
		u_char *new,off_t newsize,off_t st,off_t en,off_t *pos)
{
	off_t x,y;

	if(en-st<2) {
		x=matchlen(old+I[st],oldsize-I[st],new,newsize);
		y=matchlen(old+I[en],oldsize-I[en],new,newsize);

		if(x>y) {
			*pos=I[st];
			return x;
		} else {
			*pos=I[en];
			return y;
		}
	};

	x=st+(en-st)/2;
	if(memcmp(old+I[x],new,MIN(oldsize-I[x],newsize))<0) {
		return search(I,old,oldsize,new,newsize,x,en,pos);
	} else {
		return search(I,old,oldsize,new,newsize,st,x,pos);
	};
}

static void big_uint32out(uLong x,u_char *buf)
{
	buf[0] = (x >> 24) & 0xff;
	buf[1] = (x >> 16) & 0xff;
	buf[2] = (x >> 8) & 0xff;
	buf[3] = x & 0xff;
}

static void little_int32out(off_t x,u_char *buf)
{
	off_t y;
	if (x<0) y=-x-1; else y=x;
	if (y > 0x7fffffff) err(1, "offset too large");
	buf[3] = (y >> 24) & 0x7f;
	buf[2] = (y >> 16) & 0xff;
	buf[1] = (y >> 8) & 0xff;
	buf[0] = y & 0xff;
	if (x < 0) {
		buf[0] ^= 0xff;
		buf[1] ^= 0xff;
		buf[2] ^= 0xff;
		buf[3] ^= 0xff;
	}
}

int main(int argc,char *argv[])
{
	int fd;
	u_char *old,*new;
	off_t oldsize,newsize;
	off_t *I,*V;
	off_t scan,pos,len;
	off_t lastscan,lastpos,lastoffset;
	off_t oldscore,scsc;
	off_t s,Sf,lenf,Sb,lenb;
	off_t overlap,Ss,lens;
	off_t i;
	off_t dblen,eblen;
	u_char *db,*eb;
	u_char buf[4];
	uLong adler;

	FILE * pf;

	if(argc!=4) errx(1,"usage: %s oldfile newfile patchfile",argv[0]);

	/* Allocate oldsize+1 bytes instead of oldsize bytes to ensure
		that we never try to malloc(0) and get a NULL pointer */
	if(((fd=open(argv[1],O_RDONLY,0))<0) ||
		((oldsize=lseek(fd,0,SEEK_END))==-1) ||
		((old=malloc(oldsize+1))==NULL) ||
		(lseek(fd,0,SEEK_SET)!=0) ||
		(read(fd,old,oldsize)!=oldsize) ||
		(close(fd)==-1)) err(1,"%s",argv[1]);

	if(((I=malloc((oldsize+1)*sizeof(off_t)))==NULL) ||
		((V=malloc((oldsize+1)*sizeof(off_t)))==NULL)) err(1,NULL);

	qsufsort(I,V,old,oldsize);

	free(V);

	/* Allocate newsize+1 bytes instead of newsize bytes to ensure
		that we never try to malloc(0) and get a NULL pointer */
	if(((fd=open(argv[2],O_RDONLY,0))<0) ||
		((newsize=lseek(fd,0,SEEK_END))==-1) ||
		((new=malloc(newsize+1))==NULL) ||
		(lseek(fd,0,SEEK_SET)!=0) ||
		(read(fd,new,newsize)!=newsize) ||
		(close(fd)==-1)) err(1,"%s",argv[2]);

	if(((db=malloc(newsize+1))==NULL) ||
		((eb=malloc(newsize+1))==NULL)) err(1,NULL);
	dblen=0;
	eblen=0;

	/* Create the patch file */
	if ((pf = fopen(argv[3], "w")) == NULL)
		err(1, "%s", argv[3]);
	
	/* Header is
		0	4	big endian adler32 checksum of newfile */
	/* Block is
		0	4	X (add X bytes from oldfile to X bytes from the diff block)
		4	4	Y (copy Y bytes from the diff block)
		8	4	Z (seek forwards in oldfile by Z bytes)
		12	X	diff block
		12+X	Y	extra block */
	/* File is
		Header
		Block*
		EOF */

	/* Write header */
	adler = adler32(0L, Z_NULL, 0);
	adler = adler32(adler, new, newsize);
	big_uint32out(adler, buf);
	if (fwrite(buf, 4, 1, pf) != 1)
		err(1, "fwrite(%s)", argv[3]);

	/* Compute the differences, writing as we go */
	scan=0;len=0;
	lastscan=0;lastpos=0;lastoffset=0;
	while(scan<newsize) {
		oldscore=0;

		for(scsc=scan+=len;scan<newsize;scan++) {
			len=search(I,old,oldsize,new+scan,newsize-scan,
					0,oldsize,&pos);

			for(;scsc<scan+len;scsc++)
			if((scsc+lastoffset<oldsize) &&
				(old[scsc+lastoffset] == new[scsc]))
				oldscore++;

			if(((len==oldscore) && (len!=0)) || 
				(len>oldscore+8)) break;

			if((scan+lastoffset<oldsize) &&
				(old[scan+lastoffset] == new[scan]))
				oldscore--;
		};

		if((len!=oldscore) || (scan==newsize)) {
			s=0;Sf=0;lenf=0;
			for(i=0;(lastscan+i<scan)&&(lastpos+i<oldsize);) {
				if(old[lastpos+i]==new[lastscan+i]) s++;
				i++;
				if(s*2-i>Sf*2-lenf) { Sf=s; lenf=i; };
			};

			lenb=0;
			if(scan<newsize) {
				s=0;Sb=0;
				for(i=1;(scan>=lastscan+i)&&(pos>=i);i++) {
					if(old[pos-i]==new[scan-i]) s++;
					if(s*2-i>Sb*2-lenb) { Sb=s; lenb=i; };
				};
			};

			if(lastscan+lenf>scan-lenb) {
				overlap=(lastscan+lenf)-(scan-lenb);
				s=0;Ss=0;lens=0;
				for(i=0;i<overlap;i++) {
					if(new[lastscan+lenf-overlap+i]==
					   old[lastpos+lenf-overlap+i]) s++;
					if(new[scan-lenb+i]==
					   old[pos-lenb+i]) s--;
					if(s>Ss) { Ss=s; lens=i+1; };
				};

				lenf+=lens-overlap;
				lenb-=lens;
			};

			for(i=0;i<lenf;i++)
				db[i]=new[lastscan+i]-old[lastpos+i];
			for(i=0;i<(scan-lenb)-(lastscan+lenf);i++)
				eb[i]=new[lastscan+lenf+i];

			dblen=lenf;
			eblen=(scan-lenb)-(lastscan+lenf);

			/* X */
			little_int32out(lenf, buf);
			if (fwrite(buf, 4, 1, pf) != 1)
				err(1, "fwrite(%s)", argv[3]);

			/* Y */
			little_int32out((scan-lenb)-(lastscan+lenf),buf);
			if (fwrite(buf, 4, 1, pf) != 1)
				err(1, "fwrite(%s)", argv[3]);

			/* Z */
			little_int32out((pos-lenb)-(lastpos+lenf),buf);
			if (fwrite(buf, 4, 1, pf) != 1)
				err(1, "fwrite(%s)", argv[3]);
			
			/* diff bytes */
			if (dblen > 0 && fwrite(db, dblen, 1, pf) != 1)
				err(1, "fwrite(%s)", argv[3]);
			
			/* extra bytes */
			if (eblen > 0 && fwrite(eb, eblen, 1, pf) != 1)
				err(1, "fwrite(%s)", argv[3]);

			lastscan=scan-lenb;
			lastpos=pos-lenb;
			lastoffset=pos-scan;
		};
	};

	if (fclose(pf))
		err(1, "fclose");

	/* Free the memory we used */
	free(db);
	free(eb);
	free(I);
	free(old);
	free(new);

	return 0;
}
