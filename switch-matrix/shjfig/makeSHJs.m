close;clear;clc
load shj

assignments(:,3) = [1 1 2 2 1 2 1 2]';

types = [2 3 4];
msize=9;
colors = [0 0 0; 1 1 1];
labs = {'A','B'};
xlabs = {'Type II','Type III','Type IV'};
pdfsize = [250,75];

figure
for  i =1:3
	T = types(i);
	L = assignments(:,T);
	
	subaxis(1,3,i,'SpacingHoriz',0.18)
	hold on
	
% 	plot cube
	x=[-1	 1	 1	-1	-1	-1; 1	 1	-1	-1	 1	 1; 1	 1	-1	-1	 1	 1;-1	 1	 1	-1	-1	-1];
	y=[-1	-1	 1	 1	-1	-1;-1	 1	 1	-1	-1	-1;-1	 1	 1	-1	 1	 1;-1	-1	 1	 1	 1	 1];
	z=[-1	-1	-1	-1	-1	 1;-1	-1	-1	-1	-1	 1; 1	 1	 1	 1	-1	 1; 1	 1	 1	 1	-1	 1];
	for j=1:6
% 		h=patch(x(:,j),y(:,j),z(:,j),'linestyle','n','facealpha',0,'edgecolor','k');
		h=plot3(x(:,j),y(:,j),z(:,j),'k');
	end
	
	view([-45,-10,15])
	for j =1:2
		
		xs = stimuli(L==j,3);
		ys = stimuli(L==j,2);
		zs = stimuli(L==j,1);

		
		plot3(xs,ys,zs,'o','markersize',msize,...
			'markerfacecolor',colors(j,:),'markeredgecolor','k')
% 		text(xs,ys,zs,char(labs(j)),...
% 			'color',1-colors(j,:),'horizontalalignment','center')
		
	end
	axis square
	axis off
% 	title(xlabs(i))

	
	
	
end



set(gcf,'position',[300 200 pdfsize])
set(gcf, 'color', 'w'); set(gca, 'color', 'w');  
matlab2tikz shj.tex

export_fig shj.pdf -opengl