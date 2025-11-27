#include "vertex.h"

vertex::vertex() : edge(){

  id    =0;          
  degree=0;      
  K     =1;           
  bridge=0;   
  label =0;   //safer to set '0' than NULL.  
  E     =0;       
  Nk    =0;
  Ns    =0;
  // PRINT =false;

  Kprobs=0;
  Nsamples=0;
  
}

vertex::~vertex(){ freeSpace(); }


void vertex::copy( vertex *VV ){

  int i,length;
  
  id     = VV->id;
  degree = VV->degree;
  K      = VV->K;
  bridge = VV->bridge;

  length = strlen(VV->label); 
  label  = new char[length];
  strcpy(label,VV->label);

  assignE       ( degree );
  assignKprobs  ( VV->Nk );
  assignNsamples( VV->Ns );

  //fill
  if( Nk != 0 ){
    for(i=0; i<Nk; i++)
      Kprobs[i] = VV->Kprobs[i];
  }  

  if( Ns != 0 ){
    for(i=0; i<Ns; i++)
      Nsamples[i] = VV->Nsamples[i];
  }
  
}

void vertex::assignNsamples( int _Ns ){

  int i;

  if( _Ns >= 0 ){

    freeNsamples();
    Ns = _Ns;
  
    //Nsamples = (double*)malloc(Ns*sizeof(double));
    Nsamples = new double[Ns];
  
    for(i=0; i<Ns; i++) Nsamples[i] = 0.0;

  }
  
  
}

void vertex::assignKprobs( int _Nk ){
  
  int i;

  if( _Nk >= 0 ){

    freeKprobs();
    Nk = _Nk;
  
    //Kprobs = (double*)malloc(Nk*sizeof(double));
    Kprobs = new double[Nk];
  
    for(i=0; i<Nk; i++) Kprobs[i] = 0.0;

  }
    
}

void vertex::freeNsamples(){

  //if(Nsamples!=0 && Ns!=0) free(Nsamples);
  //if(Nsamples!=0 && Ns!=0){ delete[] Nsamples; }
  if( Nsamples!=0 ){ delete[] Nsamples; }

  Ns = 0;

}

void vertex::freeKprobs(){

  //if(Kprobs!=0 && Nk!=0) free(Kprobs);
  //if(Kprobs!=0 && Nk!=0){ delete[] Kprobs; }
  if( Kprobs!=0 ){ delete[] Kprobs; }

  Nk = 0;

}

void vertex::assignE( int _Ek ){
  
  int i;

  if( _Ek >= 0 ){

    freeE();
    degree = _Ek;
    
    //E = (edge*)malloc(degree*sizeof(edge));
    E = new edge[degree];

  }
    
}


void vertex::freeE(){   

  //if(E!=0 && degree!=0) free(E);     //as using malloc in readgml.C
  if(E!=0 && degree!=0){ delete[] E; }

  degree = 0;
}


void vertex::freeSpace(){

  freeE();
  
  //if(label!=0)  free(label); //as using malloc in readgml.C  
  if(label!=0){ delete[] label; } 

  freeNsamples();
  
  freeKprobs();

}

/*
void vertex::printV(){

  
  // if( PRINT ){ cout << "Vertex properties: " << endl; }
  // if( PRINT ){ cout << "id: " << id << ", degree " << degree << ", K " << K << ", bridge " << bridge << ", label " << label << ", E " << E << " [" << degree << "] , Kprobs " << Kprobs << " [" << Nk << "] , Nsamples " << Nsamples << " [" << Ns << "]" << endl; }
  
}

void vertex::setPrint( bool status ){
  PRINT = status;
}
*/
