## Acronyms 
- AC : AyA-Committee. 
- AV : AyA-Validator.
- BFT : Byzantine Fault Tolerance. 
- DKG : Distributed Key Generation.
- EN : EarthNode.
- ENN : EarthNode Network.
- ENNFT : EarthNode Non-Fungible-Token.
- ENO : EarthNode Operator.
- ENOPNFT : EarthNode Operator Non-Fungible-Token. 
- eUTxO :  Extended UTxO.
- FROST : Flexible Round-Optimized Schnorr Threshold Signatures. 
- GDPR : General Data Protection Regulation.
- GDST : General-Data-Settlement-Transactions. 
- L1 : Layer 1.
- L2 : Layer 2.
- Ped-DKG :  Pedersen’s Distributed Key Generation. 
- TDST :  Telco-Data-Settlement-Transactions. 
- UTxO : Unspent transaction output.
- WMbft : World Mobile Byzantine Fault Tolerance. 
- WMC : World Mobile Chain.
- WMT : World Mobile Token.

## Glossary 

- AyA : Name of the Layer 2 Sidechain.
- AyA epoch :  The period a particular AyA Committee is in charge. Its length is bound to Root of Trust.
- Delegator : An end-user allocating WMT to one or more EarthNodes.
- Financial Settlement Layer : Public blockchain network where the cus- tomers hold their financial assets.
- Staker :  An end-user allocating WMT to one or more EarthNodes. 
- Telecommunications Settlement Layer :  World Mobile blockchain network that manages and stores telecommunications data. 
- Trust Layer : Source of truth for the connection to and from AyA.

## AyA Validator Registration Specification 

*N.B* : extracted from [Aya Whitepaper](AyAWhitePaper.pdf)

- A valid ENO is in possession of an ENNFT, which allows the registration of an AyA-Validator. 
- Registrations are handled by using a smart contract on the Root of Trust blockchain. 
- On successful execution of this smart contract, a Registration UTxO is created and paid to the smart contract address. 
- A successful registration creates an ENOPNFT, which manages the registration and identifies the operator for this AV on Cardano. 

The following information is required as input for the smart contract transaction:
1. Arbitrary UTxO to be spent, used as nonce
2. UTxO containing a valid ENNFT, which can be used as the nonce input, and the ENNFT is locked in the RegistrationUTxO
3. AyA-Validator PublicKey
4. AyA-Validator Address
5. ReferenceInput AyACommitteeUTxO, which must contain the Sidechain Identity NFT
6. Link to (MpOpToken) policy for ENOPNFT minting
7. Validator-PublicKey to verify signatures on Cardano

- The smart contract verifies that the MpOpToken minting policy successfully mints a unique ENOPNFT, which has to be paid to the ENNFT sending address. 

- The minting of an ENOPNFT has to ensure a unique allocation to its ENNFT. This approach ensures that the relevant UTxOs are spent in this transaction. 

- The ENOPNFT tokenname is stored in the registration UTxO, to allow for changes and de-registration.

- The AVRegistrationContract confirms that the provided AV-PublicKey, used to identify the individual AV on Cardano, is stored in the Datum of the RegistrationUTxO and that the registration data is signed by the AV-PublicKey. 

- Additionally, the provided Sidechain Parameter Hash must match with that in the AyACommitteeUTxO’s Inline-Datum and it must contain the Identity-NFT of the sidechain for verification. The final condition is that the ENNFT must be locked on the RegistrationUTxO.

- During installation of an EarthNode, an AV is set up before registration on Root of Trust because the data needed for the registration is generated during the installation. This means, if the registration is valid, the specific AV is already present on the sidechain and waiting for authorisation to become an active AyA-Validator.

- The registration transaction is picked up by a Light-Client-Module belonging to an existing AV and a transaction is issued on the sidechain. 

- This transaction is verified by the AyA network. On approval, a newly-registered AV receives the base stake to become a validator.

- This finalises the registration on the sidechain and the new AV is eligible for block proposals as well as AC membership.

## Specification Revisions

TBD 


## Executable Specifications

- Registration Specifications
  - Nominal Cases
      - Register
      - Update 
      - Unregister                                                
  - `Property 1` : Non Fungible Property Transitivity : EN Token is an NFT => the ENOP Token should be an NFT
    - `Register` 
      - `Property 1.0` : When Minting ENOP Tokens, Tokens Quantities are verified
        - `Property. 1.0.1 violation` - No ENNOP Minted (can't be enforced it is an issue...):  
        - `Property. 1.0.2 violation` - ENNOP's Minted Quantity > 1:                             
        - `Property. 1.0.4 violation` - ENNFT 's Minted Quantity > 1:                            
      - `Property 1.1` : When Minting ENOP Tokens, NFTs Token Names & Cardinality equality : There is 1-1 relationship between the ENNFT and the ENOPNFT
        - `Property. 1.1.0 violation` - ENOPNFT TokenName =/ ENNFT TokenName:                   
        - `Property. 1.1.1 violation` - |ENOP NFT| > 1 (Cardinality Violation):                 
        - `Property. 1.1.2 violation` - |EN NFT|   > 1 (Cardinality Violation):
    - `Update`
      - only 1 EN NFT and 1 ENOP NFT are used with no minting
    - `UnRegister`                 
  - `Property 2` : Preserving NFTs ownership : ENOP and ENNFT can be swapped only between the operator and the registration smart contract
    - `Register`
      - `Property 2.0` : ENOP NFT should be minted only to the operator
        - `Property. 2.0.0 violation` - ENNOP Minted Not Output to Operator:                    
      - `Property 2.1` : Only the operator should sign the transaction
        - `Property. 2.1.0 violation` - No signer found (Enforced by Ledger Properties):        
        - `Property. 2.1.1 violation` - signer is not unique: 
      - `Property 2.2` : ENNFT should be provided by operator and spent on script
        - `Property. 1.0.3 violation` - No ENNFT on Registration validator output:               
    - `Update`
      - an only ENOP NFT should be provided by the operator
      - an only ENOP NFT should be sent back to the operator
      - an only ENNFT should be move from a registration script UTxO to another one
      - `Property 2.1` : Only the operator should sign the transaction
        - `Property. 2.1.0 violation` - No signer found (Enforced by Ledger Properties)        
        - `Property. 2.1.1 violation` - signer is not unique
    - `Unregister`                                  

  - `Property 3` : Authenticity of the registration details (datum) should be verifiable and Valid (Signed and Provided by the owner of the ENNFT)
   - `Register`
    - `Property. 3.0 violation` - Registration datum should be valid
        -  `ennftTokenName` field should be equal to the spent ennft token Name
        -  `enopNFTCurrencySymbol` field should be equal to the ENOP NFT currency Symbol minted
    - `Property. 3.0 violation` - Registration datum can not be deserialized
    - `Property. 3.0 violation` - Registration datum is not Auhtentic                              
    - `Property. 3.1 violation` - Registration validator output not found (not verifiable)                 
    - `Property. 3.2 violation` - Registration validator output has no datum (not verifiable)             
    - `Property. 3.3 violation` - Registration validator output has only the hashed datum (not verifiable)  
    - `Property. 3.4 violation` - More than one Registration validator output is not allowed (Reducing complexity)
   