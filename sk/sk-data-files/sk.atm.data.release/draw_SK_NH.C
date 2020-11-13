
void draw_SK_NH()
{
  TFile * infile = new TFile("sk1234.atmospheric.201803.nh.root");

  TGraph * sk1234_NH_S23M23_90CL = (TGraph*) infile->Get("sk1234_NH_S23M23_90CL");
  TGraph * base_NH_S23M23        = (TGraph*) infile->Get(  "base_NH_S23M23");


  TCanvas * cmain = new TCanvas("cmain", "", 800, 800 );
  cmain->SetFillColor(0);
  cmain->SetLeftMargin   (0.20);
  cmain->SetTopMargin    (0.10);
  cmain->SetBottomMargin (0.15);
  cmain->SetRightMargin  (0.10);

  base_NH_S23M23        ->Draw("AC"   ); 
  sk1234_NH_S23M23_90CL ->Draw("Csame"); 

  TLegend * leg = new TLegend( 0.70, 0.83 , 0.90, 0.90 );
  leg->SetFillColor(0);
  leg->SetBorderSize(1);

  leg->AddEntry( sk1234_NH_S23M23_90CL , "NH 90\% C.L.");

  leg->Draw("same");

  cmain->SaveAs("sk1234.m23_s23.201803.nh.pdf");


}
