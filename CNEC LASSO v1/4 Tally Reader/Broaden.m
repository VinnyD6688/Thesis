% Read in the pulse height spectrum
A=load ('C:\Users\vadinova\Documents\GitHub\MCNPncsu\CNEC LASSO v1\2 Deck Builder\Pure Decks\ClKSU14MeV_near.txt'); %change file path
B=[A(:,1),A(:,2)];
[mdim,ndim]=size(B);
Spectra=B;
Spectra(:,ndim+1)=0;
%Initialize the new spectrum
Spectra(:,ndim+1)=0
for i=2:mdim 
    %Calculate the sigma for each energy point
    sig(i)=0.031*sqrt(Spectra(i,1));  %NaI old
    %sig(i)=0.157445191*realpow(Spectra(i,1),0.73543706);%NaI(KSU)
   % sig(i)=0.03262254*realpow(Spectra(i,1),.89638972); %CeBr
  
    %Do convolution
    for j=2:mdim
        Spectra(j,ndim+1)=Spectra(j,ndim+1)+Spectra(i,2)*exp(-(Spectra(j,1)-Spectra(i,1))^2/(2*sig(i)^2))/(sqrt(2.0*3.14159)*sig(i));
    end 
end
semilogy(Spectra(:,1),Spectra(:,2),Spectra(:,1),Spectra(2,1)*Spectra(:,ndim+1))
legend('Original Spectrum','With Gaussian Spreading')
axis([0 Spectra(mdim,1) 0.000001 100000])
title('Insert title')
xlabel('Energy [MeV]')
ylabel('Relative Counts')

%Output the old spectrum and new spectrum to ph.txt file
fid=fopen('Broad.dat','w');
for i=1:mdim
    %fprintf(fid,'%10.6f %12.8f %12.8f \n ',EE8(i,1),EE8(i,2),EE8(2,1)*EE8(i,ndim+1));
    fprintf(fid,'%10.6f %12.8f \n ',Spectra(i,1),Spectra(2,1)*Spectra(i,ndim+1));
end
