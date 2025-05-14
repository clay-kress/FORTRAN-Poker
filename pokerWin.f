      implicit none
      include 'var.inc'

      integer i
      real x

      call execute_command_line('cls') !cls for windows

!Basic instructions for the game****************************************
1     format(/xxxa/)
2     format(a/,xxxxxa/)
3     format(a,a/)
      print 1, 'This is the Game of Poker. -as developed by: Clay Kress'
      print 2, '1: Players start by placing 1 chip into the pot',
     &                 'and then decide independently what to bet next.'
      print 2, '2: There are two betting rounds, in between them',
     &                                'you can replace 3 of your cards.'
      print 2, '3: The final bets must all be equal to the highest bet',
     &'although you may fold if you do not wish to bet the full amount.'
      print 2, '4: Folding will forfeit your cards and leave all',
     &         'previously bet chips in the pot for the winner to take.'
      print 3, 'Your goal is to get as many chips as possible. ',
     &                                                      'Good Luck!'
      print *, 'Press any key and enter to continue:'
      read(*,*)
!Basic instructions for the game****************************************

      call ranSeed(seed)

      print *, 'How many chips should we deal you?'
      read(*,*) x
      x= int(x)

      pchips(1)= x
      pchips(2)= x
      pchips(3)= x
      pchips(4)= x
      pturn= int(4*ran0(seed))

      call execute_command_line('cls') !cls for windows

      print *, 'Loading...'
      call sleep(2)

11    call execute_command_line('cls') !cls for windows

        pturn= pturn+1
        if (pturn .eq. 4) pturn= 0

        !Antes
        pot= 0
        do i= 1,4
          pchips(i)= pchips(i)-1
          pot= pot+1
        enddo
        do i= 1,4
          pbet(i)= 1
        enddo

        !Draws the cards
        call pcard()

        !Reads your hand
        call qhand()

        !Handles all the betting
        call cbett(1)

        !Replaces your cards
        call cswitch()

        !Reads your hand
        call qhand()

        !Handles all the betting
        call cbett(2)

        !Determines the winner
        call hwins()

      if (minval(pchips) .gt. 0) goto 11
      end

!#######################################################################

      subroutine pcard()
      implicit none
      include 'var.inc'

      integer i,j

      !52 card generator
      do i= 1,52
100     cdeck(1,i)= ceiling(52*ran0(seed))
        do j= 1,i-1
          if (cdeck(1,i) .eq. cdeck(1,j)) then
            goto 100
          endif
        enddo
      enddo

      !Fills in the suit
      do i= 1,52
        cdeck(2,i)= ceiling(float(cdeck(1,i))/13)
        cdeck(1,i)= (mod(float(cdeck(1,i)),13.0))+1
      enddo

      !Draws the cards
      do i= 1,5
        do j= 1,2
          pcards(j,i,1)= cdeck(j,i)
          pcards(j,i,2)= cdeck(j,i+5)
          pcards(j,i,3)= cdeck(j,i+10)
          pcards(j,i,4)= cdeck(j,i+15)
        enddo
      enddo

      call cardsrt()

      print *, 'Round 1 betting:'
      print *, ''
      print *, 'The pot is worth: ', pot
      print *, 'You have:         ', pchips(1), 'chips'
      print *, 'Player cards'
      do i= 1,5
        print *, 'Card:', pcards(1,i,1), 'Suit:', pcards(2,i,1)
      enddo

      return
      endsubroutine

!#######################################################################

      subroutine qhand()
      implicit none
      include 'var.inc'

      integer i
      integer hand(9),best(1)
      integer aceHigh
      integer onePair
      integer twoPair
      integer threeKind
      integer straat
      integer floosh
      integer fullHouse
      integer fourKind
      integer stfl

      !Finds the first true hand
      do i= 1,4

        hand(9)= aceHigh(i)
        hand(8)= onePair(i)
        hand(7)= twoPair(i)
        hand(6)= threeKind(i)
        hand(5)= straat(i)
        hand(4)= floosh(i)
        hand(3)= fullHouse(i)
        hand(2)= fourKind(i)
        hand(1)= stfl(i)

        cardSum(i)= pcards(1,1,i)+ pcards(1,2,i)+ pcards(1,3,i)
     &                           + pcards(1,4,i)+ pcards(1,5,i)

        best= maxloc(hand)
        phand(i)= (-1*best(1))+10
      enddo

      !Finds the actual value of the hands
      do i= 1,4
        handVal(i)= cardSum(i)**phand(i)
      enddo

      if (pbet(1) .ne. 0) then
        print *, 'Player hand: (PlayerHand, HandValue)= ',
     &                                              phand(1), handVal(1)
        print *, ''
      endif

      return
      endsubroutine

!#######################################################################

      subroutine cbett(n)
      implicit none
      include 'var.inc'

      integer i,j,k
      integer n
      integer playerBet(4)

      if (n .eq. 1) then
        do i= 1,4
          pbet(i)= 1
        enddo
      else
        do i= 1,4
          if (pbet(i) .ne. 0) pbet(i)= 1
        enddo
      endif

!Generates the bet
      do i= 1,4

        k= 0

        j= mod(i+pturn,4)!Creates the sequence (1234,2341,3412,4123)... Trust me
        if (j.eq. 0) j= 4

        if (j .eq. 1 .and. pbet(j) .ne. 0) then
          !Human
11        continue      !Requires that this happen at least once
            print *, 'Player:', pturn+1, 'bet first'
            print *, 'Minimum Bet: ', maxval(pbet)
            print *, 'What would you like to bet? (0= fold)'
            read(*,*) pbet(1)
          if (pbet(1) .lt. maxval(pbet) .and. pbet(1) .ne. 0) goto 11

        elseif (j .ne. 1 .and. pbet(j) .ne. 0) then
          !Computer
12        continue
            pbet(j)= phand(j)/1.5+ gaus0(seed)*.65
            k= k+ 1
          if (pbet(j) .lt. maxval(pbet) .and. k .lt. 1000) goto 12

          if (k .ge. 1000) pbet(j)= 0
          if (pbet(j) .lt. 0) pbet(j)= 0

        endif
      enddo

!Finishes the betting
      do i= 1,4

        j= mod(i+pturn,4)
        if (j.eq. 0) j= 4

        if (j .eq. 1) then
          !Human
          playerBet(j)= pbet(j)

          do while (playerBet(j) .ne. maxval(pbet)
     &                                        .and. playerBet(j) .ne. 0) !May skip this if betting needs are met
            print *, 'The bet has been raised to: ', maxval(pbet)
            print *, 'What would you like to bet?'
            read(*,*) playerBet(j)
            print *, ''
          enddo

          if (playerBet(j) .eq. 0) then
            pchips(j)= pchips(j)- pbet(j)
            pot= pot+ pbet(j)
            playerBet(j)= pbet(j)
            pbet(j)= 0
          else
            pbet(1)= playerBet(j)
            pchips(1)= pchips(1)- pbet(1)
            pot= pot+ pbet(1)
          endif

        elseif (j .ne. 1) then
          !Computer
          if (maxval(pbet)-pbet(j) .gt. 6.5*ran0(seed)
     &                                         .or. pbet(j) .eq. 0) then
            pchips(j)= pchips(j)- pbet(j)
            pot= pot+ pbet(j)
            playerBet(j)= pbet(j)
            pbet(j)= 0
          else
            pbet(j)= maxval(pbet)
            playerBet(j)= pbet(j)
            pchips(j)= pchips(j)- pbet(j)
            pot= pot+ pbet(j)
          endif

        endif
      enddo

!Prints the bets
      print *, ''
      print *, 'Round:', n, 'Betting:'
      call sleep(1)
      do i= 1,4
        j= mod(i+pturn,4)
        if (j.eq. 0) j= 4

        if (pbet(j) .eq. 0) then
          print *, 'Player: ', j, 'has bet: ', playerBet(j), '(fold)'
          call sleep(1)
        else
          print *, 'Player: ', j, 'has bet: ', playerBet(j)
          call sleep(1)
        endif
      enddo
      print *, ''
      print *, 'The pot is worth: ', pot
      call sleep(7)

      return
      endsubroutine

!#######################################################################

      subroutine hwins()
      implicit none
      include 'var.inc'

      integer i
      integer winning(1)

      call execute_command_line('cls') !cls for windows

      do i= 1,4
        if (pbet(i) .eq. 0) then
          handVal(i)= 0
        endif
      enddo

      winning= maxloc(handVal)

      if (winning(1) .eq. 0) then
        winning(1)= pturn
        if (winning(1) .eq. 0) winning(1)= 4
      endif

      win= winning(1)

      pchips(win)= pchips(win)+ pot
      pot= 0

      print *, ''
      print *, ''
      print *, 'Player: ', win,
     &        'Has won this round with a handVal of:',
     &                                phand(win), ',', handVal(win), '!'
      print *, ''
      do i= 1,4
        print *, '             Player:', i, ',', pchips(i), 'Chips!'
      enddo
      call sleep(10)

      return
      endsubroutine

!#######################################################################

      subroutine cswitch()
      implicit none
      include 'var.inc'

      integer i,j,n,t

      call execute_command_line('cls') !cls for windows
      if (pbet(1) .ne. 0) then
        print *, 'Replacement:'
        print *, ''
        print *, 'The pot is worth: ', pot
        print *, 'You have:         ', pchips(1), 'chips'
        print *, 'Player cards'
        do i= 1,5
          print *, 'Card:', pcards(1,i,1), 'Suit:', pcards(2,i,1)
        enddo
      endif
      print *, ''

      do n= 1,4

        t= mod(n+pturn,4)
        if (t .eq. 0) t= 4

        if (t .eq. 1 .and. pbet(t) .ne. 0) then

          print *, 'Which cards would you like to replace?'
          print *, 'Enter 3 numbers referencing the order of the card,',
     &         ' using 0s to indicate no card Ex:1,3,0'
          read(*,*) crep(1), crep(2), crep(3)
          do i= 1,3
            do j= 1,2
              pcards(j,crep(i),t)= cdeck(j,17+(t*3)+i)
            enddo
          enddo
          print *, 'Wait...'

        elseif (t .ne. 1 .and. pbet(t) .ne. 0) then

          do i= 1,5
            if (coi(i,t) .eq. 1) nrep= nrep+ 1
          enddo
          do i= 1,nrep
            do j= 1,5
              if (coi(j,t).eq. 1 .and.nint(.3*gaus0(seed)+2).eq. 2) then
                crep(i)= j
                coi(j,t)= 0
                exit
              elseif (coi(j,t).eq. 1) then
                crep(i)= 0
                coi(j,t)= 0
                exit
              else
                crep(i)= 0
              endif
            enddo
          enddo
          call arrsrt(crep,3)
          nrep= 3
          do i= 1,3
            if (crep(i) .eq. 0) nrep= nrep- 1
          enddo
          do i= 1,nrep
            do j= 1,2
              pcards(j,crep(i),t)= cdeck(j,17+(t*3)+i)
            enddo
          enddo

        endif
      enddo
      call cardsrt()
      call sleep(2)

      call execute_command_line('cls') !cls for windows
      if (pbet(1) .ne. 0) then
        print *, 'Round 2 betting:'
        print *, ''
        print *, 'The pot is worth: ', pot
        print *, 'You have:         ', pchips(1), 'chips'
        print *, 'Player cards'
        do i= 1,5
          print *, 'Card:', pcards(1,i,1), 'Suit:', pcards(2,i,1)
        enddo
      endif

      return
      endsubroutine

!#######################################################################
!Poker sorting function
!pcards(suit,num,player)

      subroutine cardsrt()
      implicit none
      include 'var.inc'
            integer i,j,k,l,x

            do i= 1,4

              do j= 1,5
                do k= 1,5

                  if (pcards(1,j,i) .lt. pcards(1,k,i)) then
                    do l= 1,2

                      x= pcards(l,j,i)
                      pcards(l,j,i)= pcards(l,k,i)
                      pcards(l,k,i)= x
                    enddo
                  endif

                enddo
              enddo

            enddo

      return
      endsubroutine

!#######################################################################
!Array sort

      subroutine arrsrt(arr,n)
      implicit none
            integer n
            integer arr(n)
            integer i,j,x

            do i= 1,n
                  do j= 2,n
                        if (arr(j) .gt. arr(j-1)) then
                              x= arr(j)
                              arr(j)= arr(j-1)
                              arr(j-1)= x
                        endif
                  enddo
            enddo

      return
      endsubroutine
!#######################################################################

c Generates a seed based on the date and time
      subroutine ranSeed(idum)
            implicit none
            integer*4 Tval(3),Dval(3)
            integer idum
            integer i

            idum= 0
            call itime(Tval)
            call idate(Dval)
            do i= 1,3
              idum= idum+Tval(i)
              idum= idum*Dval(i)
            enddo
      return
      endsubroutine

c Numerical Rec. Random Number Generator
c output= ran0(seed)
      real function ran0(idum)
            implicit none
            integer idum,IA,IM,IQ,IR,MASK
            integer k
            real AM
            PARAMETER (IA=16807,IM=2147483647,AM=1./IM,
     &                 IQ=127773,IR=2836,MASK=123459876)
c Minimal random number generator of Park and Miller.
c Returns a uniform random deviate between 0.0 and 1.0.
c Set or reset idum to any integer value except the unlikely value MASK
c to initialize the sequence; idum must not be altered between calls for
c successive deviates in a sequence.
            idum=ieor(idum,MASK)
            k=idum/IQ
            idum=IA*(idum-k*IQ)-IR*k
            if (idum.lt.0) idum=idum+IM
            ran0=AM*idum
            idum=ieor(idum,MASK)
      return
      endfunction


c Returns a normally distributed deviate with zero mean and unit variance, using ran0(idum)
c as the source of uniform deviates.
c 3+gaus0(idum) changes the mean
c gaus0(idum)*3 changes the standard deviation
      real function gaus0(idum)
            integer idum,iset
            real fac,gset,rsq,v1,v2,ran0
            save iset,gset
            data iset/0/
            if (iset.eq.0) then
1             v1=2.*ran0(idum)-1.
              v2=2.*ran0(idum)-1.
              rsq=v1**2+v2**2
              if(rsq.ge.1..or.rsq.eq.0.)goto 1
              fac=sqrt(-2.*log(rsq)/rsq)
              gset=v1*fac
              gaus0=v2*fac
              iset=1
            else
              gaus0=gset
              iset=0
            endif
      return
      endfunction

!#######################################################################
      integer function aceHigh(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            aceHigh= 0
            aceHigh= 1

            coi(1,player)= 1
            coi(2,player)= 1
            coi(3,player)= 1
            coi(4,player)= 0
            coi(5,player)= 0
      return
      endfunction
!#######################################################################
      integer function onePair(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            onePair= 0

            do i= 1,5
              do j= 1,5
                if (pcards(1,i,player) .eq. pcards(1,j,player)
     &                                              .and. i .ne. j) then

                  onePair= 1

                  !Cards to throw away
                  do n= 1,5

                    if (n .ne. i .and. n .ne. j) then
                      coi(n,player)= 1
                    else
                      coi(n,player)= 0
                    endif
                  enddo
                  !!!
                  return
                endif
              enddo
            enddo
      return
      endfunction
!#######################################################################
      integer function twoPair(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            twoPair= 0

            do i= 1,5
              do j= 1,5

                do k= 1,5
                  do l= 1,5
                    if (pcards(1,i,player) .eq. pcards(1,j,player)
     &            .and. pcards(1,k,player) .eq. pcards(1,l,player)
     &                .and. i .ne. j .and. i .ne. k .and. i .ne. l
     &                               .and. j .ne. k .and. j .ne. l
     &                                              .and. k .ne. l) then

                      twoPair= 1

                      !Cards to throw away
                      do n= 1,5

                        if (n .ne. i .and. n .ne. j .and.
     &                      n .ne. k .and. n .ne. l) then
                          coi(n,player)= 1
                        else
                          coi(n,player)= 0
                        endif
                      enddo
                      !!!
                      return
                    endif
                  enddo
                enddo

              enddo
            enddo
      return
      endfunction
!#######################################################################
      integer function threeKind(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            threeKind= 0

            do i= 1,5
              do j= 1,5
                do k= 1,5
                  if (pcards(1,i,player) .eq. pcards(1,j,player)
     &          .and. pcards(1,j,player) .eq. pcards(1,k,player)
     &                               .and. i .ne. j .and. i .ne. k
     &                                              .and. j .ne. k) then

                    threeKind= 1

                    !Cards to throw away
                    do n= 1,5

                      if (n .ne. i .and. n .ne. j .and. n .ne. k) then
                        coi(n,player)= 1
                      else
                        coi(n,player)= 0
                      endif
                    enddo
                    !!!
                    return
                  endif
                enddo
              enddo
            enddo
      return
      endfunction
!#######################################################################
      integer function straat(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            straat= 0

            do i= 1,5
              do j= 1,5
                if (pcards(1,i,player) .eq. pcards(1,j,player)
     &                                              .and. i .ne. j) then

                  return
                endif
              enddo
            enddo

            if (maxval(pcards(1,:,player))
     &         -minval(pcards(1,:,player)) .eq. 4) then

              straat= 1

              !Cards to throw away
              do n= 1,5
                coi(n,player)= 0
              enddo
              !!!
              return
            endif
      return
      endfunction
!#######################################################################
      integer function floosh(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            floosh= 0

            if (minval(pcards(2,:,player))
     &     .eq. maxval(pcards(2,:,player))) then

              floosh= 1

              !Cards to throw away
              do n= 1,5
                coi(n,player)= 0
              enddo
              !!!
              return
            endif
      return
      endfunction
!#######################################################################
      integer function fullHouse(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            fullHouse= 0

            do i= 1,5
              do j= 1,5
                do k= 1,5

                  do l= 1,5
                    do m= 1,5

                      if (pcards(1,i,player) .eq. pcards(1,j,player)
     &              .and. pcards(1,i,player) .eq. pcards(1,k,player)
     &              .and. pcards(1,l,player) .eq. pcards(1,m,player)
     & .and. i .ne. j .and. i .ne. k .and. i .ne. l .and. i .ne. m
     &                .and. j .ne. k .and. j .ne. l .and. j .ne. m
     &                               .and. k .ne. l .and. k .ne. m
     &                                              .and. l .ne. m) then

                        fullHouse= 1

                        !Cards to throw away
                        do n= 1,5
                          coi(n,player)= 0
                        enddo
                        !!!
                        return
                      endif

                    enddo
                  enddo

                enddo
              enddo
            enddo
      return
      endfunction
!#######################################################################
      integer function fourKind(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            fourKind= 0

            do i= 1,5
              do j= 1,5
                do k= 1,5
                  do l= 1,5
                    if (pcards(1,i,player) .eq. pcards(1,j,player)
     &            .and. pcards(1,j,player) .eq. pcards(1,k,player)
     &            .and. pcards(1,k,player) .eq. pcards(1,l,player)
     &                .and. i .ne. j .and. i .ne. k .and. i .ne. l
     &                               .and. j .ne. k .and. j .ne. l
     &                                              .and. k .ne. l) then

                      fourKind= 1

                      !Cards to throw away
                      do n= 1,5

                        if (n .ne. i .and. n .ne. j .and.
     &                      n .ne. k .and. n .ne. l) then
                          coi(n,player)= 1
                        else
                          coi(n,player)= 0
                        endif
                      enddo
                      !!!
                      return
                    endif
                  enddo
                enddo
              enddo
            enddo
      return
      endfunction
!#######################################################################
      integer function stfl(player)
      implicit none
            include 'var.inc'
            integer player
            integer i,j,k,l,m,n

            stfl= 0

            if (minval(pcards(2,:,player))
     &     .ne. maxval(pcards(2,:,player))) then

              return !Cuts the function, floosh= 0
            endif

            do i= 1,5
              do j= 1,5
                if (pcards(1,i,player) .eq. pcards(1,j,player)
     &                                              .and. i .ne. j) then

                  return
                endif
              enddo
            enddo

            if (maxval(pcards(1,:,player))
     &         -minval(pcards(1,:,player)) .ne. 4) then

              return !Cuts the function, straat= 0
            endif

            stfl= 1

            !Cards to throw away
            do n= 1,5
              coi(n,player)= 0
            enddo
            !!!
      return
      endfunction
