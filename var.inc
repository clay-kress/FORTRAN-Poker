      real gaus0,ran0
      integer seed
      
      integer cdeck(2,52)
      integer pcards(2,5,4)
      
      integer phand(4)
      integer cardSum(4)
      integer handVal(4)
      
      integer pbet(4)
      integer pchips(4)
      
      integer nrep
      integer crep(3)
      integer coi(5,4)
      
      integer pturn
      integer pot
      integer win
      
      common /pok/cdeck,pcards,phand,cardSum,handVal,pbet,pchips,
     &                                             nrep,crep,coi,pot,win
      
      common /manager/seed,pturn
      
