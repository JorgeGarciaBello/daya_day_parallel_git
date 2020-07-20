# reno_surface_pull_cov
#data_pull=load("reno_data.dat");
#data_cov=load("reno_data_cov.dat");

data_pull=load("reno_data_cal.dat");
data_cov=load("reno_data_cov_cal.dat");


minimio=min(data_pull(:,3))
data_pull(:,3)=data_pull(:,3)-minimio;
k=1;l=1;m=1;
for i=1:122500
	if(data_pull(i,3)<=2.3)
	  cf_68(k,:)=[data_pull(i,1),data_pull(i,2)];
	  k=k+1;
	elseif(data_pull(i,3)<=6.18)
	  cf_95(l,:)=[data_pull(i,1),data_pull(i,2)];
	  l=l+1;
	elseif(data_pull(i,3)<=11.83)
	  cf_99(m,:)=[data_pull(i,1),data_pull(i,2)];
	  m=m+1;
	endif
end

minimio=min(data_cov(:,3))
data_cov(:,3)=data_cov(:,3)-minimio;
k=1;l=1;m=1;
for i=1:900
	if(data_cov(i,3)<=2.3)
	  cf_68_cov(k,:)=[data_cov(i,1),data_cov(i,2)];
	  k=k+1;
	elseif(data_cov(i,3)<=6.18)
	  cf_95_cov(l,:)=[data_cov(i,1),data_cov(i,2)];
	  l=l+1;
	elseif(data_cov(i,3)<=11.83)
	  cf_99_cov(m,:)=[data_cov(i,1),data_cov(i,2)];
	  m=m+1;
	endif
end

cf_68=cf_68'; cf_95=cf_95'; cf_99=cf_99';
cf_68_cov=cf_68_cov'; cf_95_cov=cf_95_cov'; cf_99_cov=cf_99_cov';

file = fopen('reno_confidence_region_pull_68.dat', 'w'); fprintf(file,'%d %d \n' ,cf_68); fclose(file);
file = fopen('reno_confidence_region_pull_95.dat', 'w'); fprintf(file,'%d %d \n' ,cf_95); fclose(file);
file = fopen('reno_confidence_region_pull_99.dat', 'w'); fprintf(file,'%d %d \n' ,cf_99); fclose(file);

file = fopen('reno_confidence_region_cov_68.dat', 'w'); fprintf(file,'%d %d \n' ,cf_68_cov); fclose(file);
file = fopen('reno_confidence_region_cov_95.dat', 'w'); fprintf(file,'%d %d \n' ,cf_95_cov); fclose(file);
file = fopen('reno_confidence_region_cov_99.dat', 'w'); fprintf(file,'%d %d \n' ,cf_99_cov); fclose(file);

cl_68=load("reno2018_cl68.dat");
cl_95=load("reno2018_cl95.dat");
cl_99=load("reno2018_cl99.dat");


cf_68_pull=load("reno_confidence_region_pull_68.dat");
cf_95_pull=load("reno_confidence_region_pull_95.dat");
cf_99_pull=load("reno_confidence_region_pull_99.dat");

cf_68_cov=load("reno_confidence_region_cov_68.dat");
cf_95_cov=load("reno_confidence_region_cov_95.dat");
cf_99_cov=load("reno_confidence_region_cov_99.dat");

plot(cf_68_pull(:,1),cf_68_pull(:,2),'h',"markersize", 2,"linewidth",3,
     cf_95_pull(:,1),cf_95_pull(:,2),'h',"markersize", 2,"linewidth",3,
     cf_99_pull(:,1),cf_99_pull(:,2),'h',"markersize", 2,"linewidth",3,
     cf_68_cov(:,1),cf_68_cov(:,2),'h',"markersize", 5,"linewidth",5,
     cf_95_cov(:,1),cf_95_cov(:,2),'h',"markersize", 5,"linewidth",5,
     cf_99_cov(:,1),cf_99_cov(:,2),'h',"markersize", 5,"linewidth",5,
     cl_68(:,1),cl_68(:,2),'h',"markersize", 1,"linewidth",0,
     cl_95(:,1),cl_95(:,2),'h',"markersize", 1,"linewidth",0,
     cl_99(:,1),cl_99(:,2),'h',"markersize", 1,"linewidth",0);

axis ([0.0, 0.2, 0.0017, 0.003501]); ylabel('\Deltam^{2}_{ee}');xlabel('sin^2 2\theta_{13}');