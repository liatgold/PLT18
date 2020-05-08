{
  let internal_fsig, fsig_size := getfSig(input_ptr)
  let array_length := mload(add(input_ptr, fsig_size))
  let values_ptr := add(add(input_ptr, fsig_size), 32)
  
  // build returned array - add length
  mstore(output_ptr, array_length)
  
  // internal_output_ptr is the memory pointer
  // at which the next array element is stored
  let internal_output_ptr := add(output_ptr, 32)
  result_length := 32
  for { let i:= 0 } lt(i, array_length) { i := add(i, 1) } {
    // Execute function on array element
    let interm_res_length := executeInternal(
      internal_fsig,
      values_ptr,
      32,
      internal_output_ptr,
      virtual_fns
    )
  // Move array values pointer & the resulting array pointer
    // to the next element
    values_ptr := add(values_ptr, 32)
    internal_output_ptr := add(internal_output_ptr, interm_res_length)
    
  // Update final result length
    result_length := add(result_length, interm_res_length)
  }
}
