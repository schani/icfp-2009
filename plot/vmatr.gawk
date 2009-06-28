
(NR==1)			{ for (i=1; i<NF; i++) X[i]=$(i+1); XD=NF; next; }
(NR>1 && NF==XD)	{ Y[++j]=$1;
			  for (i=1; i<NF; i++) V[i,j]=$(i+1); }

END			{ YD=++j;
			  /* printf("%dx%d\n", XD, YD); */ 
			  
			  for (i=1; i<XD; i++) {
			    for (j=1; j<YD; j++) {
			      printf("%f %f %f\n", X[i], Y[j], V[i,j]);
			    }
			    printf("\n");
			  }    
			} 
