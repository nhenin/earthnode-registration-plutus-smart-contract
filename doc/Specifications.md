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

### Register  
A valid Registration produces a an NFT (ENOPNFT), therefore the Register Action is under the responsability of the ENOPNFT Minting Script.

### Properties for Minting the ENOPNFT 

An ENOPNFT is only minted if its corresponding ENNFT is locked on the script with a valid EnRegistration :    

1. The ENNFT is on the UTxO value that will be stored in the Right Validator Registration Script
2. `enopnft.tokenName == ennft.tokenName`
2. Datum is signed with AV PubKey contained in the Datum (using ZK1 Signatures)
3. ENRegistration Datum is valid 
```haskell
data EnRegistration = EnRegistration
    { -- | Owner Account which is operating the Aya Validator on the Aya chain  
      enOperatorAddress :: BuiltinByteString
      -- | Validator Pubkey of the consensus Node
    , enConsensusPubKey :: BuiltinByteString
      -- | MerkleRoot for verifiable randomness (this will disappear)
    , enMerkleTreeRoot :: BuiltinByteString
      -- | CrossChain Pubkey which can be verified on both chains
    , enCceAddress :: BuiltinByteString
      -- | Unique ENNFT name, "used" for this registration
    , enUsedNftTn :: TokenName
      -- | Operator's wallet where rewards will be delivered after participating in a block production in Aya
    , enRwdWallet :: PubKeyHash
      -- | Commission in percent shared with staking delegators.
    , enCommission :: Integer
      -- | We cannot store the EnOpNft CurrencySymbol in the parameter because we get a cyclic ?dependency
      -- Nicolas Henin : This should be removed, it can be retrieved Offchain 
    , pEnOpCs :: CurrencySymbol 
      -- | Signature of the datum. All datum fields concatenated and signed by the enCceAddress  
    , enSignature :: BuiltinByteString
    }
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)
```
 - Not Clear : How Do we validate sidechain params : 
    - ReferenceInput AyACommitteeUTxO, which must contain the Sidechain Identity NFT ? 
    - Sidechain Parameter Hash must match with that in the AyACommitteeUTxO’s Inline-Datum and it must contain the Identity-NFT of the sidechain for verification
 - `datum.enUsedNftTn.tokenName == enopnft.tokenName == value.ennft.tokenName`


 ## Update 

 ## Unregistrer