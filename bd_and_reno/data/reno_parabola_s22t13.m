# reno_parabola_s22t13
data=load('parabola_s22t13_reno_data_cal.dat');
data1=load('parabol_s2_reno.dat');

val_min=min(data1(:,2));
for i=1,45
	data1(:,2)=data1(:,2)-val_min;
end
plot(data(:,1),data(:,2),"linewidth",1.5,'r',
	 data1(:,1),data1(:,2),"*",'markersize',4.5);
xlabel('sin^{2} 2\theta_{13}');
ylabel('\Delta \chi^2');
axis ([0.0,0.2, 0.0,7]);

set(gca, 'xtick', [0.05,0.1,0.15,0.2]);
h = legend ({ "Data from statistical analysis","Data from RENO"},"location", "southwest");
set(h,"fontsize",10);
legend boxoff