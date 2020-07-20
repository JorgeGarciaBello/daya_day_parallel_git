# reno_parabola_dmee
data=load('parabola_dmee_reno_data_cal.dat');
data1=load('parabol_dmee_reno.dat');

val_min=min(data1(:,2));
for i=1,92
	data1(:,2)=data1(:,2)-val_min;
end
plot(data(:,1),data(:,2),"linewidth",1.5,'r',
	 data1(:,1),data1(:,2),"*",'markersize',4.5);
xlabel('\Deltam^{2}_{ee}   [eV^{2}]');
ylabel('\Delta \chi^2');
axis ([0.002, 0.0035,0.0,7]);

set(gca, 'xtick', [0.002,0.0025,0.0030,0.0035]);
set(gca, 'xticklabel', {'2.0x10^{-3}','2.5x10^{-3}','3.0x10^{-3}','3.5x10^{-3}'} );

h = legend ({ "Data from statistical analysis","Data from RENO"},"location", "east");
legend boxoff

