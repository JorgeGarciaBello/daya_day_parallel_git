%grid_3D=load('g.th23th13dm.CKM.3M.dat_XB.Sep6_M');
%grid_3D=load('g.th23th13dm.sk.3M.dat_XB.Sep7_M');
grid_3D=load('sk-db-reno-minos-ji-nh.dat');


print_chi2=1        % condicion para imprimir las tablas de chi2 
print_allowed=1
plot_chi2=1
plot_allowed_fix=1
plot_allowed_free=1

dummy=size(grid_3D);


points=50
l=1;
for i=1:points
for j=1:points
for k=1:points

%write(theta_23_control,*) XXX(1),XXX(2),XXX(3),F
dm(i)=grid_3D(l,1);
th13(j)=grid_3D(l,2);
th23(k)=grid_3D(l,3);
chi2(i,j,k)=grid_3D(l,4);

l=l+1;
end
end
end

interpts=1000;

del_th13=(th13(points)-th13(1))/(interpts-1);
del_th23=(th23(points)-th23(1))/(interpts-1);
del_dm=(dm(points)-dm(1))/(interpts-1);

a=points;

[Xq,Yq,Zq]=meshgrid(dm(1):del_dm:dm(a),th13(1):del_th13:th13(a),th23(1):del_th23:th23(a));


dm_1=dm(1);
th13_1=th13(1);
th23_1=th23(1);


for i=1:interpts
dm_int(i)=dm_1;
th13_int(i)=th13_1    ;
th23_int(i)=th23_1 ;    
dm_1=dm_1+del_dm;
th13_1=th13_1+del_th13;
th23_1=th23_1+del_th23;


end    


chi2_int = interp3(dm,th13,th23,chi2,Xq,Yq,Zq,'spline');


%parpool('threads')

%for i=1:interpts
%for j=1:interpts
%for k=1:interpts
%Xq1=dm_int(i);
%Yq1=th13_int(j);
%Zq1=th23_int(k);
%chi2_int(i,j,k) = interp3(dm,th13,th23,chi2,Xq1,Yq1,Zq1);
%end
%end
%end




minimum_int=min(min(min(chi2_int)))

minimum=min(min(min(chi2)))

for i=1:a
chi2_dm(i)=min(min(chi2(i,:,:)))-minimum;
chi2_th13(i)=min(min(chi2(:,i,:)))-minimum;
chi2_th23(i)=min(min(chi2(:,:,i)))-minimum;
end


for i=1:interpts
chi2_dm_int(i)=min(min(chi2_int(i,:,:)))-minimum_int;
chi2_th13_int(i)=min(min(chi2_int(:,i,:)))-minimum_int;
chi2_th23_int(i)=min(min(chi2_int(:,:,i)))-minimum_int;

end




if (plot_chi2==1)

figure ('Name',' chi2-dm ')
plot(dm,chi2_dm)
hold on
plot(dm_int,chi2_dm_int)

ylim([0.0 10.0])


figure ('Name','chi2-th13 ')
plot(th13,chi2_th13)


hold on
plot(th13_int,chi2_th13_int)
ylim([0.0 10.0])




figure('Name','chi2-th23 ')
plot(th23,chi2_th23)
hold on
plot(th23_int,chi2_th23_int)
ylim([0.0 10.0])


end

if (print_chi2 ==1 ) 


chi2_dm2=[dm',chi2_dm'];
chi2_dm2_int=[dm_int',chi2_dm_int'];

chi2_theta13=[th13',chi2_th13'];
chi2_theta13_int=[th13_int',chi2_th13_int'];

chi2_theta23=[th23',chi2_th23'];
chi2_theta23_int=[th23_int',chi2_th23_int'];


dlmwrite('chi2_dm2_SK.txt',chi2_dm2 )
dlmwrite('chi2_dm2_int_SK.txt',chi2_dm2_int )
dlmwrite('chi2_theta13_SK.txt',chi2_theta13)
dlmwrite('chi2_theta13_int_SK.txt',chi2_theta13_int)
dlmwrite('chi2_theta23_SK.txt',chi2_theta23)
dlmwrite('chi2_theta23_int_SK.txt',chi2_theta23_int)


end



for k=1:a
minim_k(k)=min(min(chi2(:,:,k)));
end
min_tot=min(minim_k);
kmin=find(minim_k==min_tot);
[imin,jmin]=find(chi2(:,:,kmin)==min_tot);
chi2(imin,jmin,kmin)
dm(imin)
th13(jmin)
th23(kmin)



for k=1:interpts
minim_k_int(k)=min(min(chi2_int(:,:,k)));
end
min_tot_int=min(minim_k_int);


kmin_int=find(minim_k_int==min_tot_int);
[imin_int,jmin_int]=find(chi2_int(:,:,kmin_int)==min_tot_int);
chi2_int(imin_int,jmin_int,kmin_int);
dm_int(imin_int);
th13_int(jmin_int);
th23_int(kmin_int);


chi2_int_Zero=chi2_int-min_tot_int;

%chi2_int_Zero(imin_int,jmin_int,kmin_int)

%contour3(sin2th2_int,mass_int,Zq_zero,[2,4,9])

% ALLOWED REGION TH13 VS TH23
%FIXING DM23= DM23_MINIMUN= 2.5D-3

i=imin_int ;
mm_1=1;
ll_1=1;
kk_1=1;

mm_2=1;
ll_2=1;
kk_2=1;

mm_3=1;
ll_3=1;
kk_3=1;

for j=1:interpts
    for k=1:interpts

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
        chi=chi2_int_Zero(imin_int,j,k);
        
        if (  (chi<=2.0) )
        cl1_th13_1(kk_1)=th13_int(j) ;   
        cl1_th23_1(kk_1)=th23_int(k)   ;
        kk_1=kk_1+1;
        end
                
        if (  (chi<=4.6) )
        cl1_th13_2(ll_1)=th13_int(j) ;   
        cl1_th23_2(ll_1)=th23_int(k)   ;
        ll_1=ll_1+1;
        end        
        
         if (  (chi<=9.0) )
        cl1_th13_3(mm_1)=th13_int(j) ;   
        cl1_th23_3(mm_1)=th23_int(k)   ;         
        mm_1=mm_1+1;
        end     
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
        
        chi=chi2_int_Zero(j,jmin_int,k);
        
        if (  (chi<=2.0) )
        cl2_dm_1(kk_2)=dm_int(j)*1000 ;   
        cl2_th23_1(kk_2)=th23_int(k)   ;
        kk_2=kk_2+1;
        end
                
        if (  (chi<=4.6) )
        cl2_dm_2(ll_2)=dm_int(j)*1000 ;   
        cl2_th23_2(ll_2)=th23_int(k)   ;
        ll_2=ll_2+1;
        end        
        
         if (  (chi<=9.0) )
        cl2_dm_3(mm_2)=dm_int(j)*1000 ;   
        cl2_th23_3(mm_2)=th23_int(k)   ;         
        mm_2=mm_2+1;
        end     
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        


        
        chi=chi2_int_Zero(j,k,kmin_int);
        
        if (  (chi<=2.0) )
        cl3_dm_1(kk_3)=dm_int(j)*1000 ;   
        cl3_th13_1(kk_3)=th13_int(k)   ;
        kk_3=kk_3+1;
        end
                
        if (  (chi<=4.6) )
        cl3_dm_2(ll_3)=dm_int(j)*1000 ;   
        cl3_th13_2(ll_3)=th13_int(k)   ;
        ll_3=ll_3+1;
        end        
        
         if (  (chi<=9.0) )
        cl3_dm_3(mm_3)=dm_int(j)*1000 ;   
        cl3_th13_3(mm_3)=th13_int(k)   ;         
        mm_3=mm_3+1;
        end     
        

            
     end
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

if (plot_allowed_fix==1)

figure ('Name','Allowed th13 vs th23  dm=fixed@min ')
plot (cl1_th13_3,cl1_th23_3,'* k')
hold on
plot (cl1_th13_2,cl1_th23_2,'* r')
hold on
plot (cl1_th13_1,cl1_th23_1,'* g')


figure ('Name','Allowed th23 vs dm23  th13=fixed@min   ')
plot (cl2_th23_3,cl2_dm_3,'* k')
hold on
plot (cl2_th23_2,cl2_dm_2,'* r')
hold on
plot (cl2_th23_1,cl2_dm_1,'* g')



figure ('Name','Allowed th13 vs dm23  th23=fixed@min  ')
plot (cl3_th13_3,cl3_dm_3,'* k')
hold on
plot (cl3_th13_2,cl3_dm_2,'* r')
hold on
plot (cl3_th13_1,cl3_dm_1,'* g')

end


if (print_allowed==1)

cl95_th13th23_fix=[cl1_th13_3',cl1_th23_3'];
cl90_th13th23_fix=[cl1_th13_2',cl1_th23_2'];
cl68_th13th23_fix=[cl1_th13_1',cl1_th23_1'];

dlmwrite('cl95_th13th23_fix.SK.txt',cl95_th13th23_fix )
dlmwrite('cl90_th13th23_fix.SK.txt',cl90_th13th23_fix )
dlmwrite('cl68_th13th23_fix.SK.txt',cl68_th13th23_fix )


cl95_th23dm23_fix=[cl2_th23_3',cl2_dm_3'];
cl90_th23dm23_fix=[cl2_th23_2',cl2_dm_2'];
cl68_th23dm23_fix=[cl2_th23_1',cl2_dm_1'];

dlmwrite('cl95_th23dm23_fix.SK.txt',cl95_th23dm23_fix )
dlmwrite('cl90_th23dm23_fix.SK.txt',cl90_th23dm23_fix )
dlmwrite('cl68_th23dm23_fix.SK.txt',cl68_th23dm23_fix )

cl95_th13dm23_fix=[cl3_th13_3',cl3_dm_3' ];
cl90_th13dm23_fix=[cl3_th13_2',cl3_dm_2' ];
cl68_th13dm23_fix=[cl3_th13_1',cl3_dm_1' ];

dlmwrite('cl95_th13dm23_fix.SK.txt',cl95_th13dm23_fix )
dlmwrite('cl90_th13dm23_fix.SK.txt',cl90_th13dm23_fix )
dlmwrite('cl68_th13dm23_fix.SK.txt',cl68_th13dm23_fix )


end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


for i=1:interpts
for j=1:interpts

grid_dm23th13(i,j)=min(chi2_int_Zero(i,j,:));
grid_dm23th23(i,j)=min(chi2_int_Zero(i,:,j));
grid_th13th23(i,j)=min(chi2_int_Zero(:,i,j));

end
end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%allowed region with out fixing the thrid paramenter 

mm_1=1;
ll_1=1;
kk_1=1;

mm_2=1;
ll_2=1;
kk_2=1;

mm_3=1;
ll_3=1;
kk_3=1;

for j=1:interpts
    for k=1:interpts

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
%        chi=chi2_int_Zero(imin_int,j,k);
        chi=grid_th13th23(j,k);
        
        
        if (  (chi<=2.0) )
        cl1_th13_M1(kk_1)=th13_int(j) ;   
        cl1_th23_M1(kk_1)=th23_int(k)   ;
        kk_1=kk_1+1;
        end
                
        if (  (chi<=4.6) )
        cl1_th13_M2(ll_1)=th13_int(j) ;   
        cl1_th23_M2(ll_1)=th23_int(k)   ;
        ll_1=ll_1+1;
        end        
        
         if (  (chi<=9.0) )
        cl1_th13_M3(mm_1)=th13_int(j) ;   
        cl1_th23_M3(mm_1)=th23_int(k)   ;         
        mm_1=mm_1+1;
        end     
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        
        
%        chi=chi2_int_Zero(j,jmin_int,k);
        chi=grid_dm23th23(j,k);
        
        if (  (chi<=2.0) )
        cl2_dm_M1(kk_2)=dm_int(j)*1000 ;   
        cl2_th23_M1(kk_2)=th23_int(k)   ;
        kk_2=kk_2+1;
        end
                
        if (  (chi<=4.6) )
        cl2_dm_M2(ll_2)=dm_int(j)*1000 ;   
        cl2_th23_M2(ll_2)=th23_int(k)   ;
        ll_2=ll_2+1;
        end        
        
         if (  (chi<=9.0) )
        cl2_dm_M3(mm_2)=dm_int(j)*1000 ;   
        cl2_th23_M3(mm_2)=th23_int(k)   ;         
        mm_2=mm_2+1;
        end     
        
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%        


        
%        chi=chi2_int_Zero(j,k,kmin_int);
        chi=grid_dm23th13(j,k);
        
        if (  (chi<=2.0) )
        cl3_dm_M1(kk_3)=dm_int(j)*1000 ;   
        cl3_th13_M1(kk_3)=th13_int(k)   ;
        kk_3=kk_3+1;
        end
                
        if (  (chi<=4.6) )
        cl3_dm_M2(ll_3)=dm_int(j)*1000 ;   
        cl3_th13_M2(ll_3)=th13_int(k)   ;
        ll_3=ll_3+1;
        end        
        
         if (  (chi<=9.0) )
        cl3_dm_M3(mm_3)=dm_int(j)*1000 ;   
        cl3_th13_M3(mm_3)=th13_int(k)   ;         
        mm_3=mm_3+1;
        end     
        

            
     end
    end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

if (plot_allowed_free==1)

figure ('Name','Allowed th13 vs th23  dm=FREE')
plot (cl1_th13_M3,cl1_th23_M3,'* k')
hold on
plot (cl1_th13_M2,cl1_th23_M2,'* r')
hold on
plot (cl1_th13_M1,cl1_th23_M1,'* g')


figure ('Name','Allowed th23 vs dm23  th13=FREE   ')
plot (cl2_th23_M3,cl2_dm_M3,'* k')
hold on
plot (cl2_th23_M2,cl2_dm_M2,'* r')
hold on
plot (cl2_th23_M1,cl2_dm_M1,'* g')



figure ('Name','Allowed th13 vs dm23  th23=FREE ')
plot (cl3_th13_M3,cl3_dm_M3,'* k')
hold on
plot (cl3_th13_M2,cl3_dm_M2,'* r')
hold on
plot (cl3_th13_M1,cl3_dm_M1,'* g')

end


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


if (print_allowed==1)



cl95_th13th23_free=[cl1_th13_M3',cl1_th23_M3'];
cl90_th13th23_free=[cl1_th13_M2',cl1_th23_M2'];
cl68_th13th23_free=[cl1_th13_M1',cl1_th23_M1'];

dlmwrite('cl95_th13th23_free.SK.txt',cl95_th13th23_free )
dlmwrite('cl90_th13th23_free.SK.txt',cl90_th13th23_free )
dlmwrite('cl68_th13th23_free.SK.txt',cl68_th13th23_free )


cl95_th23dm23_free=[cl2_th23_M3',cl2_dm_M3'];
cl90_th23dm23_free=[cl2_th23_M2',cl2_dm_M2'];
cl68_th23dm23_free=[cl2_th23_M1',cl2_dm_M1'];

dlmwrite('cl95_th23dm23_free.SK.txt',cl95_th23dm23_free )
dlmwrite('cl90_th23dm23_free.SK.txt',cl90_th23dm23_free )
dlmwrite('cl68_th23dm23_free.SK.txt',cl68_th23dm23_free )

cl95_th13dm23_free=[cl3_th13_M3',cl3_dm_M3' ];
cl90_th13dm23_free=[cl3_th13_M2',cl3_dm_M2' ];
cl68_th13dm23_free=[cl3_th13_M1',cl3_dm_M1' ];

dlmwrite('cl95_th13dm23_free.SK.txt',cl95_th13dm23_free )
dlmwrite('cl90_th13dm23_free.SK.txt',cl90_th13dm23_free )
dlmwrite('cl68_th13dm23_free.SK.txt',cl68_th13dm23_free )


end


%xlim([0.065 0.1])
%ylim([2.2  2.8])

