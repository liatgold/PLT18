object "SimpleStore" {
  code {
    datacopy(0, dataoffset("Runtime"), datasize("Runtime"))
    return(0, datasize("Runtime"))
  }
  
       
  object "Runtime" {
    code {
        function increment(a) -> result
        {
            result:=add(a,1)
        }
        function cal(a) -> result
        {
            
            if lt(a, 3)
            {
                result := 3
            }
            if lt(3, a)
            {
                result := 4
            }
        }


      calldatacopy(0, 0, 36) // write calldata to memory, start from 0, in total 36 bytes
      // example call data: 6057361d0000000000000000000000000000000000000000000000000000000000000002
        //6057361d is function signature(4 bytes)
        //0000000000000000000000000000000000000000000000000000000000000002 is the input variable(32 bytes uint256)
        let fn_sig := mslice(0, 4)  //0-4 is function signature
        
        switch fn_sig
    

      case sig"function store(uint256 val) public" { // new signature method
        let memory_ptr:=4
        let tmp:=mload(memory_ptr) //load memory 4
        tmp:=cal(tmp)
        sstore(0,tmp) //store to storage 0
        
      }

      case sig"function get() view public returns (uint256)" {
          mstore(100, sload(0)) //write to memory 100 in total 32 bytes
          return(100,32) //return memory pointer to memory 100
      }

    }
  }
}
