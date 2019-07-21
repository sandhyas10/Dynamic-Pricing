p1=c(8,7,4,2,2); #Descending order of price
v1=c(0.6,0.5,0.8,0.9,0.1);
p2=c(9,7,6,5,4);
v2=c(0.7,0.2,0.2,0.6,0.8);
p3=c(10,8,5,3,1);
v3=c(0.1,0.1,0.5,0.8,1);
r1=0;
r2=0;
r3=0;
z.l=0;
z.u=15; 
v.1.asrt= 1:5
for(i in (1:length(v1)))
{
  sum=0;
  for(j in 1:i)
  {
    sum=sum+v1[j];
    }
  v.1.asrt[i]= sum;    
}


r.1.asrt=1:5
for( i in (1:length(v1)))
{
  sum=0;
  for( j in 1:i)
  {
    sum=sum+p1[j]*v1[j]/v.1.asrt[i];
  }
  r.1.asrt[i]= sum;
}


v.2.asrt= 1:5
for(i in (1:length(v2)))
{
  sum=0;
  for(j in 1:i)
  {
  sum=sum+v2[j];
  }
  v.2.asrt[i]= sum;    
}

r.2.asrt=1:5
for(i in (1:length(v2)))
{
  sum=0;
  for(j in 1:i)
  {
    sum=sum+p2[j]*v2[j]/v.2.asrt[i];
  }
r.2.asrt[i]= sum;
}

v.3.asrt= 1:5
for(i in (1:length(v3)))
{
  sum=0;
  for(j in 1:i)
  {
  sum=sum+v3[j];
  }
  v.3.asrt[i]= sum;    
}

r.3.asrt=1:5
for(i in (1:length(v3)))
{
  sum=0;
  for(j in 1:i)
  {
  sum=sum+p3[j]*v3[j]/v.3.asrt[i];
  }
r.3.asrt[i]= sum;
}


while (z.u-z.l>0.0001)
{
  z=(z.u+z.l)/2;
  ass_1=(v.1.asrt^0.5)*(r.1.asrt-zhat);
  opt_ass1=max(ass_1);
  no_optass1= which(ass_1==opt_ass1)
  ass_2=(v.2.asrt^0.5)*(r.2.asrt-zhat);
  opt_ass2=max(ass_2);
  no_optass2= which(ass_2==opt_ass2)
  ass_3=(v.3.asrt^0.5)*(r.3.asrt-zhat);
  opt_ass3=max(ass_3);
  no_optass3= which(ass_3==opt_ass3)
  optimal=opt_ass1+opt_ass2+opt_ass3;
  if(z>=opt){
    z.u=z;
  }
  else
  {
    z.l=z;
  }
}


for(k in 1:no_optass1)
{
rev1=rev1+(v.1.asrt[no_optass1]^0.5)/(1+v.2.asrt[no_optass1]^0.5+v.2.asrt[no_optass2]^0.5+v.3.asrt[no_optass3]^0.5)*(p1[k]*v_1sort[k])/v.1.asrt[no_optass1];
}

for(k in 1:no_optass2)
{
rev2=rev2+(v.2.asrt[no_optass2]^0.5)/(1+v.1.asrt[no_optass1]^0.5+v.2.asrt[no_optass2]^0.5+v.3.asrt[no_optass3]^0.5)*(p2[k]*v2[k])/v.2.asrt[no_optass2];
}


for(k in 1:no_optass3)
{
  rev3=rev3+(v.3.asrt[no_optass3]^0.5)/(1+v.1.asrt[no_optass1]^0.5+v.2.asrt[no_optass2]^0.5+v.3.asrt[no_optass3]^0.5)*(p3[k]*v3[k])/v.3.asrt[no_optass3];
}
